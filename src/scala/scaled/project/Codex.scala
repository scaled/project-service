//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.{Extractor, SourceSet, TextWriter, Writer}
import codex.model.{Def, Element, Kind, Flavor, Ref, Relation, Sig, Source}
import codex.store.{ProjectStore, MapDBStore, Query}
import java.nio.file.{Path, Files}
import java.util.{ArrayList, Optional, LinkedHashSet, HashMap}
import scaled._
import scaled.code.CodeConfig
import scaled.util.{BufferBuilder, Errors, FuzzyMatch}

/** Static [[Codex]] stuff. */
object Codex {

  /** Returns the `Codex` associated with `buffer`. */
  def apply (buffer :Buffer) :Codex = buffer.state.get[Codex].getOrElse {
    throw new IllegalStateException(s"No codex configured in buffer: '$buffer'")
  }

  /** Returns the `Codex` associated with `editor`. */
  def apply (editor :Editor) :Codex = editor.state[Codex].getOrElse {
    throw new IllegalStateException(s"No Codex configured in editor!")
  }

  /** Converts a `Source` to a `Store`. */
  def toStore (source :Source) :Store = Store(source.toString)
  // Source's toString representation is the same for expected by Store.apply

  /** Convert a `Store` to a `Source`. */
  def toSource (store :Store) :Source = store match {
    case fs  :FileStore     => new Source.File(fs.path.toString)
    case zes :ZipEntryStore => new Source.ArchiveEntry(zes.zipFile.toString, zes.entry)
    case _ => throw new IllegalArgumentException(s"Can't convert $store to Codex Source.")
  }
}

/** Used to report Codex info inside a project. */
class CodexComponent (codex :Codex, store :CodexStore) extends Project.Component {

  override def addToBuffer (buffer :RBuffer) :Unit = {
    // while this buffer is open, keep an up to date SourceIndex in its state
    val conn = codex.indexed.onValue { idx =>
      if (idx.store == buffer.store) buffer.state[SourceIndex]() = idx
    }
    buffer.killed.onEmit { conn.close() }
  }

  /** Appends a description of this store to `bb`. */
  override def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addSubHeader("Codex:")
    bb.add(Line.builder(s"Defs: ${store.defCount}").withLineTag(Visit.Tag(new Visit() {
      protected def go (window :Window) = CodexSummaryMode.visitTopLevel(window, store)
    })).build())
  }
}

/** A [[MapDBStore]] with some extra integration with Scaled bits. */
class CodexStore (val root :Project.Root, indexFile :Path) extends MapDBStore(
  root.toString, indexFile) {

  /** Returns true if this store has no defs. */
  def isEmpty () = !topLevelDefs.iterator.hasNext
}

/** Manages code indexes for projects, using the Codex index service.
  * @see https://github.com/scaled/codex
  */
class Codex (editor :Editor, msvc :MetaService) {
  import Codex._

  private lazy val codexDir = Files.createDirectories(msvc.metaFile("Codex"))
  private val storesByRoot = new HashMap[Project.Root, CodexStore]()
  private val storesById = new HashMap[Project.Id, CodexStore]()
  private def psvc = msvc.service[ProjectService]

  /** A signal emitted when a source has been reprocessed and indexed. */
  val indexed = Signal[SourceIndex]()

  /** An execution queue on which Codex indexing is run. This serializes all indexing actions which
    * avoids grinding a user's machine to a halt with multiple full indexes. */
  val indexQueue :Pipe[Unit] = msvc.process(())

  /** Closes all of our open stores in preparation for editor shutdown. */
  def close () :Unit = {
    storesByRoot.values.foreach { _.close() }
  }

  /** Returns the Codex store for `project`. */
  def store (project :Project) :CodexStore = store(project.root)

  /** Returns the Codex store for the project in `root`. */
  def store (root :Project.Root) :CodexStore = {
    val store = storesByRoot.get(root)
    if (store != null) store
    else {
      val indexDir = Files.createDirectories(codexDir.resolve(root.hashName))
      val newStore = try new CodexStore(root, indexDir.resolve("index"))
      catch {
        case e :Throwable =>
          msvc.log.log(s"Failed to create store in ${indexDir}", e)
          throw e
      }
      storesByRoot.put(root, newStore)
      newStore
    }
  }

  /** Returns the Codex store for project `id` if such a project can be resolved. */
  def store (id :Project.Id) :Option[CodexStore] =
    Option(storesById.get(id)) orElse psvc.resolveById(id).map(root => {
      val storeByRoot = store(root)
      storesById.put(id, storeByRoot)
      storeByRoot
    })

  /** Returns the store for `project`, stores for all top-level in `project`'s workspace, and all
    * the stores for the dependencies of `project`. */
  def stores (project :Project) :LinkedHashSet[ProjectStore] = {
    val stores = new LinkedHashSet[ProjectStore]()
    def checkedAdd (store :CodexStore) :Unit = {
      // if this store is empty; trigger a resolution of its project to cause it to be indexed for
      // the first time
      if (store.isEmpty) {
        project.emitStatus(s"Triggered index of ${store.root}")
        project.pspace.projectFor(store.root)
      }
      stores.add(store)
    }
    checkedAdd(store(project))
    project.pspace.allProjects map(_._1) filter(_ != project.root) map(store) foreach(checkedAdd)
    project.depends.ids flatMap(id => store(id)) foreach(checkedAdd)
    stores
  }

  /** Removes the database for the Codex store in `root`. */
  def deleteStore (project :Project) :Unit = {
    val store = storesByRoot.remove(project.root)
    if (store != null) {
      val iter = storesById.entrySet.iterator
      while (iter.hasNext) if (iter.next().getValue == store) iter.remove()
      store.close()
    }
    Files.deleteIfExists(codexDir.resolve(project.root.hashName).resolve("index"))
  }

  /** Describes the state of the Codex. */
  def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addHeader("Codex")
    bb.addBlank()

    bb.addSubHeader("Indices by ID:")
    bb.addKeysValues(storesById.entrySet.
      map(entry => { (s"${entry.getKey} ", entry.getValue.defCount) }).
      toSeq.sortBy(-_._2))

    // val idedRoots = storesById.values.map(_.root).toSet
    bb.addSubHeader("Indices by Root:")
    bb.addKeysValues(storesByRoot.entrySet/*.filterNot(entry => idedRoots(entry.getKey))*/.
      map(entry => { (s"${entry.getKey} ", entry.getValue.defCount) }).
      toSeq.sortBy(-_._2))
  }

  /** Returns a completer on elements of `kind` in `project`'s Codex. */
  def completer (project :Project, kind :Kind) :Completer[Def] = new Completer[Def]() {
    override def minPrefix = 2
    def complete (glob :String) = Future.success({
      val elems = glob.split(":", 2) match {
        case Array(name      ) => query(name)
        case Array(name, path) => FuzzyMatch(path).filterBy(query(name))(_.qualifier)
      }
      Completion(glob, elems, false)(formatDef)
    })
    private def query (name :String) =
      (Query.prefix(name) kind(kind) find(stores(project))).toSeqV
    private def formatDef (df :Def) = {
      // TEMP hack to distinguish Scala objects from classes in completions;
      // not exactly sure how to best handle this in a less hacky way... punt!
      val suff = if (df.flavor == Flavor.OBJECT) " (object)" else ""
      s"${df.name}$suff:${df.qualifier}"
    }
  }

  /** Resolves `ref`, which originated from a file in `project`. */
  def resolve (project :Project, ref :Ref) :Option[Def] =
    Option.from(Ref.resolve(stores(project), ref))

  /** Visits the source of `df` in a buffer in `window`. */
  def visit (window :Window, df :Def) :Unit = {
    val view = window.focus.visitFile(toStore(df.source))
    view.point() = view.buffer.loc(df.offset)
  }

  /** Applies `fn` to the the `Element` at `loc` in `buffer`, if known. Otherwise throws a feedback
    * exception indicating that no element could be found at `loc`. */
  def onElemAt[A] (buffer :Buffer, loc :Loc)(fn :(Element, Loc, Def) => A) :A = {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => buffer.state.get[SourceIndex].flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => throw Errors.feedback("No element could be found at the point.")
      case Some((elem, loc)) => resolve(buffer.state.req[Project], elem.ref) match {
        case None => throw Errors.feedback(s"Unable to resolve referent for $elem")
        case Some(df) => fn(elem, loc, df)
      }
    }
  }

  /** Returns the CSS style to use for `kind`. */
  def styleFor (kind :Kind) = kind match {
    case Kind.MODULE => Some(CodeConfig.moduleStyle)
    case Kind.TYPE   => Some(CodeConfig.typeStyle)
    case Kind.FUNC   => Some(CodeConfig.functionStyle)
    case Kind.VALUE  => Some(CodeConfig.variableStyle)
    case _           => None
  }

  /** Formats the supplied signature with colorizations and such. */
  def formatSig (sig :Sig, indent :String) :Seq[LineV] = {
    val lines = Seq.builder[LineV]()
    Line.onLines(sig.text) { (l, lstart) =>
      val len = l.length
      val lb = Line.builder(indent + l)
      for (el <- sig.uses) {
        val off = el.offset - lstart
        if (off >= 0 && off < len) styleFor(el.kind) foreach {
          val start = indent.length+off ; val end = start+el.length
          s => lb.withStyle(s, start, end).withTag(el, start, end)
        }
      }
      lines += lb.build()
    }
    lines.build()
  }

  import DocFormatterPlugin.Format

  /** Resolves the documentation for `df`. If `df` has no documentation, this will search for
    * inherited documentation for any def which `df` `Relation.OVERRIDES`.
    */
  def resolveDoc (stores :Iterable[ProjectStore], docr :DocReader, df :Def) :Format = {
    def refDoc (ref :Ref) = Option.from(Ref.resolve(stores, ref)) flatMap(findDoc)
    def relDoc (refs :Iterable[Ref]) :Option[Format] = {
      val iter = refs.iterator() ; while (iter.hasNext) {
        val doc = refDoc(iter.next)
        if (doc.isDefined) return doc
      }
      None
    }
    def findDoc (df :Def) :Option[Format] = Option.from(df.doc) match {
      case Some(doc) =>
        val docf = psvc.docFormatter(df.source.fileExt)
        Some(docf.format(df, doc, docr.resolve(df.source, doc)))
      case None => relDoc(df.relations(Relation.OVERRIDES))
    }
    findDoc(df) getOrElse DocFormatterPlugin.NoDoc
  }

  /** Formats doc and signature information into a buffer builder and returns it. */
  def summarizeDef (view :BufferView, stores :Iterable[ProjectStore], df :Def) :BufferBuilder = {
    val bb = new BufferBuilder(view.width()-2)
    val fmt = resolveDoc(stores, new DocReader(), df)
    try fmt.full("", bb)
    catch {
      case e :Exception => bb.add(Line.fromText(e.toString))
    }
    df.sig.ifPresent(new java.util.function.Consumer[Sig]() {
      def accept (sig :Sig) = bb.add(formatSig(sig, ""))
    })
    bb
  }

  /** Creates a popup for `df` including sig and docs at `loc`. */
  def mkDefPopup (view :BufferView, stores :Iterable[ProjectStore], df :Def, loc :Loc) :Popup = {
    val bb = summarizeDef(view, stores, df)
    if (bb.lines.isEmpty) bb.add(s"No docs or sig for '${df.name}'")
    Popup.lines(bb.lines, Popup.UpRight(loc))
  }

  /** Creates a debug popup for `df` at `loc`. */
  def mkDebugPopup (df :Def, loc :Loc) :Popup = {
    def safeGet (thunk : => Any) = try thunk.toString catch { case t :Throwable => t.toString }
    val text = SeqBuffer[String]()
    text += s"ID:    ${df.idToString}"
    text += s"Outer: ${df.outerIdToString}"
    text += s"Kind:  ${df.kind}"
    text += s"Exp:   ${df.exported}"
    text += s"Name:  ${df.name}"
    text += s"Off:   ${df.offset}"
    text += s"Body:  ${df.bodyStart}:${df.bodyEnd}"
    text += s"Src:   ${safeGet(df.source)}"
    text += s"GID:   ${safeGet(df.globalRef)}"
    Popup.text(text, Popup.UpRight(loc))
  }

  /** Performs any "project just got loaded/reloaded" stuffs needed by the Codex system. */
  def checkProject (project :Project) :Unit = {
    val (pstore, isEmpty) = {
      val pstore = store(project)
      try {(pstore, pstore.isEmpty)} catch {
        case ex :Throwable =>
          val msg = s"Codex store corrupt for ${project.name}. Resetting..."
          project.emitError(new Exception(msg, ex))
          deleteStore(project)
          (store(project), true)
      }
    }

    // map the project's store by its ids
    project.ids.foreach { id => storesById.put(id, pstore) }

    // add the project store to the project as a component
    project.addComponent(classOf[CodexComponent], new CodexComponent(this, pstore))

    // queue an initial reindex of this project if needed (we check for non-empty project.ids
    // because the very first time a project is resolved it will not have processed its metadata
    // and probably won't know where its source code is; so we wait for it to report back that it
    // has figured itself out before we attempt to index it)
    if (isEmpty && !project.ids.isEmpty) queueReindexAll(project)
  }

  /** Requests that `project`'s code be fully reindexed. */
  def queueReindexAll (project :Project) :Unit = {
    indexQueue.tell(_ => reindexAll(project))
  }

  /** Requests that `store` be reindexed by this project's Codex. This requests that the code be
    * reindexed by the Codex and then that a [[SourceIndex]] be generated for the source. This will
    * be done in the background, and when the indexing is complete, a new `SourceIndex` instance
    * will be emitted via [[indexed]].
    * @param force if true, file is indexed regardless of whether its last modified time is more
    * recent than the file's last recorded index.
    */
  def queueReindex (project :Project, store :Store, force :Boolean) :Unit = {
    // invoke the reindex in the background
    indexQueue.tell(_ => reindex(project, toSource(store), force))
  }

  /** Performs a debug reindex of store, writing the output to stdout. */
  def debugReindex (project :Project, store :Store) :Unit = {
    if (System.console == null) throw Errors.feedback("No console, can't emit debug output.")
    val source = toSource(store)
    extractor(project, source.fileExt) foreach { ex =>
      ex.process(SourceSet.create(source), new TextWriter(System.console.writer))
    }
  }

  /** Performs a full reindex of this project. This method is called on a background thread. */
  protected def reindexAll (project :Project) :Unit = {
    val pstore = store(project)
    pstore.clear()
    project.sources.summarize foreach { (suff, srcs) =>
      extractor(project, suff) foreach { extr =>
        project.emitStatus(s"Reindexing ${srcs.size} $suff files in ${project.name}...")
        try {
          extr.process(srcs, pstore.writer)
          project.emitStatus(s"Reindex of ${project.name} $suff files complete.")
        } catch {
          case ex :Exception =>
            project.emitStatus(s"Reindex of ${project.name} $suff files failed.")
            project.emitError(ex)
        }
      }
    }
  }

  /** Performs the actual reindexing of `source`. This method is called on a background thread. */
  protected def reindex (project :Project, source :Source, force :Boolean) :Unit = {
    val pstore = store(project)
    if (force || source.lastModified > pstore.lastIndexed(source)) {
      extractor(project, source.fileExt) foreach { ex =>
        project.metaSvc.log.log(s"Reindexing: $source")
        ex.process(SourceSet.create(source), pstore.writer)
      }
    } // else println(s"Source up to date: $source")
    reindexComplete(project, source)
  }

  /** Called when reindexing of a source file is complete. Reindexing takes place on a background
    * thread, and this method is called therefrom. */
  protected def reindexComplete (project :Project, source :Source) :Unit = {
    val ib = SourceIndex.builder(toStore(source))
    if (store(project).visit(source, ib)) project.pspace.wspace.exec.runOnUI {
      indexed.emit(ib.build())
    }
    // TODO: until we know what file extensions are known to the indexer, this generates too many
    // spurious warnings to be useful
    // else msvc.log.log(s"ProjectStore claims ignorance of just-indexed source? $source")
  }

  /** Resolves the project store for a dependency for which a Scaled project was unavailable.
    * If `None` is returned, this dependency will be omitted from the Codex. */
  protected def resolveNonProjectStore (depend :Project.Id) :Option[CodexStore] = None

  private def extractor (project :Project, suff :String) :Option[Extractor] = {
    val iter = extractorPlugins.plugins.filter(_.suffs.contains(suff)).iterator()
    while (iter.hasNext()) { // blah
      val exo = iter.next.extractor(project, suff)
      if (exo.isDefined) return exo
    }
    None
  }
  private lazy val extractorPlugins = msvc.service[PluginService].
    resolvePlugins[ExtractorPlugin]("codex-extractor")
}
