//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model.{Def, Kind, Source}
import codex.store.{MapDBStore, ProjectStore}
import java.util.ArrayList
import reactual.Signal
import scaled._
import scaled.util.VisitStack

/** [[ProjectCodex]] helpers and whatnot. */
object ProjectCodex {

  /** Converts a `Source` to a `Store`. */
  def toStore (source :Source) :Store = Store(source.toString)
  // Source's toString representation is the same for expected by Store.apply

  /** Convert a `Store` to a `Source`. */
  def toSource (store :Store) :Source = store match {
    case fs :FileStore => new Source.File(fs.path.toString)
    case zes :ZipEntryStore => new Source.ArchiveEntry(zes.zipFile.toString, zes.entry)
    case _ => throw new IllegalArgumentException(s"Can't convert $store to Codex Source.")
  }
}

/** Integrates [[Codex]] more nicely with Scaled. */
class ProjectCodex (project :Project) extends Codex with AutoCloseable {
  import ProjectCodex._

  /** The store that indexes this project's code. */
  lazy val projectStore = createProjectStore()

  /** The stores included in this Codex. */
  override lazy val stores = resolveProjectStores

  /** A signal emitted when a source has been reprocessed and indexed. */
  val indexed = Signal[SourceIndex]()

  /** A stack used to track where we've gone when using visit-element. */
  val visitStack = new VisitStack("Element visit")

  override def close () {
    // TODO: replace projectStore with Closes.Box to avoid situation where we resolve at close time
    // just to shut the projectStore down
    projectStore.close()
  }

  /** Requests that `store` be reindexed by this project's Codex. This requests that the code be
    * reindexed by the Codex and then that a [[SourceIndex]] be generated for the source. This will
    * be done in the background, and when the indexing is complete, a new `SourceIndex` instance
    * will be emitted via [[indexed]].
    */
  def reindex (store :Store) {
    // invoke the reindex in the background
    project.metaSvc.exec.runInBG { reindex(toSource(store)) }
  }

  /** Returns a completer on elements of `kind` in this project's Codex. */
  def completer (kind :Kind) :Completer[Def] = new Completer[Def]() {
    import scala.collection.JavaConversions._
    def complete (prefix :String) :Completion[Def] = prefix.split(":", 2) match {
      case Array(name, path) => elemComp(find(Codex.Query.name(name).kind(kind)) filter(
        e => Completer.startsWithI(path)(pathString(e))))
      case Array(name      ) => elemComp(find(Codex.Query.prefix(name).kind(kind)))
    }
    private def elemComp (es :Seq[Def]) = completion(es, elemToString)
    private def pathString (d :Def) = d.qualifier
    private val elemToString = (e :Def) => s"${e.name}:${pathString(e)}"
  }

  /** Visits the source of `df` in a buffer in `editor`. Pushes `curview` onto the visit stack. */
  def visit (editor :Editor, curview :BufferView, df :Def) {
    visitStack.push(curview) // push current loc to the visit stack
    val view = editor.visitFile(toStore(df.source))
    view.point() = view.buffer.loc(df.offset)
  }

  /** Displays a summary of `df` in a new buffer. Pushes `curview` onto the visit stack. */
  def summarize (editor :Editor, curview :BufferView, df :Def) {
    visitStack.push(curview) // push current loc to the visit stack
    val view = editor.createBuffer(s"${df.name}:${df.qualifier}", true,
                                   ModeInfo("codex-summary", List(Some(df), project)))
    editor.visitBuffer(view.buffer)
  }

  /** Performs the actual reindexing of `source`. This should call [[reindexComplete]] when the
    * indexing is complete.
    */
  protected def reindex (source :Source) {
    // by default we do nothing; WE HAVE NO INTELLIGENCE!
  }

  /** Called by subclasses to indicate that reindexing of a source file is complete. Reindexing
    * takes place on a background thread, and this method should also be called from the background
    * thread. */
  protected def reindexComplete (source :Source) {
    val ib = SourceIndex.builder(toStore(source))
    if (projectStore.visit(source, ib)) project.metaSvc.exec.runOnUI { indexed.emit(ib.build()) }
    else project.metaSvc.log.log(s"ProjectStore claims ignorance of just-indexed source? $source")
  }

  protected def createProjectStore () :ProjectStore = new MapDBStore(project.metaFile("codex"))

  /** Resolves the project stores for our Codex. */
  protected def resolveProjectStores :ArrayList[ProjectStore] = {
    val list = new ArrayList[ProjectStore]()
    list.add(projectStore)
    for (dep <- project.depends) project.depend(dep) match {
      case Some(p) => list.add(p.codex.projectStore)
      case None => resolveNonProjectStore(dep) foreach(list.add)
    }
    list
  }

  /** Resolves the project store for a dependency for which a Scaled project was unavailable. If
    * `None` is returned, this dependency will be omitted from the Codex. */
  protected def resolveNonProjectStore (depend :Project.Id) :Option[ProjectStore] = None
}
