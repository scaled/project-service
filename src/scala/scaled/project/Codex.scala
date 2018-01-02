//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.{Extractor, SourceSet, TextWriter, Writer}
import codex.model.{Def, Kind, Ref, Source}
import codex.store.{ProjectStore, Query}
import java.nio.file.{Path, Files}
import java.util.{ArrayList, Optional, LinkedHashSet, HashMap}
import scaled._
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
  def close () {
    storesByRoot.values.foreach { _.close() }
  }

  /** Returns the Codex store for `project. */
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
    Option(storesById.get(id)) orElse psvc.resolveById(id).map(seed => {
      val storeByRoot = store(seed.root)
      storesById.put(id, storeByRoot)
      storeByRoot
    })

  /** Returns the store for `project`, stores for all top-level in `project`'s workspace, and all
    * the stores for the dependencies of `project`. */
  def stores (window :Window, project :Project) :LinkedHashSet[ProjectStore] = {
    val stores = new LinkedHashSet[ProjectStore]()
    def checkedAdd (store :CodexStore) {
      // if this store is empty; trigger a resolution of its project to cause it to be indexed for
      // the first time
      if (store.isEmpty) {
        window.emitStatus(s"Triggered index of ${store.root}")
        project.pspace.projectIn(store.root)
      }
      stores.add(store)
    }
    checkedAdd(store(project))
    project.pspace.allProjects map(_._1) filter(_ != project.root) map(store) foreach(checkedAdd)
    project.depends flatMap(id => store(id)) foreach(checkedAdd)
    stores
  }

  /** Removes the database for the Codex store in `root`. */
  def deleteStore(project :Project) {
    storesByRoot.remove(project.root)
    Files.deleteIfExists(codexDir.resolve(project.root.hashName).resolve("index"))
  }

  /** Describes the state of the Codex. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader("Codex")
    bb.addBlank()

    bb.addSubHeader("Indices by ID:")
    bb.addKeysValues(storesById.entrySet.
      map(entry => { (s"${entry.getKey} ", entry.getValue.defCount) }).
      toSeq.sortBy(-_._2))

    val idedRoots = storesById.values.map(_.root).toSet
    bb.addSubHeader("Indices by Root:")
    bb.addKeysValues(storesByRoot.entrySet.filterNot(entry => idedRoots(entry.getKey)).
      map(entry => { (s"${entry.getKey} ", entry.getValue.defCount) }).
      toSeq.sortBy(-_._2))
  }

  /** Returns a completer on elements of `kind` in `project`'s Codex. */
  def completer (window :Window, project :Project, kind :Kind) :Completer[Def] =
    new Completer[Def]() {
      override def minPrefix = 2
      def complete (glob :String) = Future.success({
        val elems = glob.split(":", 2) match {
          case Array(name      ) => query(name)
          case Array(name, path) => FuzzyMatch(path).filterBy(query(name))(_.qualifier)
        }
        Completion(glob, elems, false)(e => s"${e.name}:${e.qualifier}")
      })
      private def query (name :String) =
        (Query.prefix(name) kind(kind) find(stores(window, project))).toSeqV
    }

  /** Resolves `ref`, which originated from a file in `project`. */
  def resolve (window :Window, project :Project, ref :Ref) :Option[Def] =
    Option.from(Ref.resolve(stores(window, project), ref))

  /** Visits the source of `df` in a buffer in `window`. */
  def visit (window :Window, df :Def) {
    val view = window.focus.visitFile(toStore(df.source))
    view.point() = view.buffer.loc(df.offset)
  }

  /** Performs any "project just got loaded/reloaded" stuffs needed by the Codex system. */
  def checkProject (project :Project) {
    val pstore = store(project)

    val isEmpty = try pstore.isEmpty catch {
      case ex :Throwable =>
        val msg = s"Codex store corrupt for ${project.name}. Resetting..."
        project.emitError(new Exception(msg, ex))
        deleteStore(project)
        true
    }

    // map the project's store by its ids
    project.ids.foreach { id => storesById.put(id, pstore) }

    // add the project store to the project as a component
    project.addComponent(classOf[CodexComponent], new CodexComponent(pstore))

    // queue an initial reindex of this project if needed (we check for non-empty project.ids
    // because the very first time a project is resolved it will not have processed its metadata
    // and probably won't know where its source code is; so we wait for it to report back that it
    // has figured itself out before we attempt to index it)
    if (isEmpty && !project.ids.isEmpty) queueReindexAll(project)
  }

  /** Requests that `project`'s code be fully reindexed. */
  def queueReindexAll (project :Project) {
    indexQueue.tell(_ => reindexAll(project))
  }

  /** Requests that `store` be reindexed by this project's Codex. This requests that the code be
    * reindexed by the Codex and then that a [[SourceIndex]] be generated for the source. This will
    * be done in the background, and when the indexing is complete, a new `SourceIndex` instance
    * will be emitted via [[indexed]].
    * @param force if true, file is indexed regardless of whether its last modified time is more
    * recent than the file's last recorded index.
    */
  def queueReindex (project :Project, store :Store, force :Boolean) {
    // invoke the reindex in the background
    indexQueue.tell(_ => reindex(project, toSource(store), force))
  }

  /** Performs a debug reindex of store, writing the output to stdout. */
  def debugReindex (project :Project, store :Store) {
    if (System.console == null) throw Errors.feedback("No console, can't emit debug output.")
    val source = toSource(store)
    extractor(project, source.fileExt) foreach { ex =>
      ex.process(SourceSet.create(source), new TextWriter(System.console.writer))
    }
  }

  /** Performs a full reindex of this project. This method is called on a background thread. */
  protected def reindexAll (project :Project) {
    val pstore = store(project)
    val sums = project.summarizeSources
    pstore.clear()
    sums foreach { (suff, srcs) =>
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
  protected def reindex (project :Project, source :Source, force :Boolean) {
    val pstore = store(project)
    if (force || source.lastModified > pstore.lastIndexed(source)) {
      extractor(project, source.fileExt) foreach { ex =>
        project.emitStatus(s"Reindexing: $source")
        ex.process(SourceSet.create(source), pstore.writer)
      }
    } // else println(s"Source up to date: $source")
    reindexComplete(project, source)
  }

  /** Called when reindexing of a source file is complete. Reindexing takes place on a background
    * thread, and this method is called therefrom. */
  protected def reindexComplete (project :Project, source :Source) {
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
