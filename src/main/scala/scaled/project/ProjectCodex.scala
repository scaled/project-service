//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model.{Kind, Source}
import codex.store.{EphemeralStore, ProjectStore}
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

  protected def createProjectStore () :ProjectStore = new EphemeralStore()

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
