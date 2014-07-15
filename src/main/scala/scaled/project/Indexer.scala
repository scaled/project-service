//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.Writer
import codex.model.Source
import reactual.Signal
import scaled._

/** Handles indexing source files, for Codex. */
class Indexer (val project :Project) {

  /** A signal emitted when a source has been reprocessed and indexed. */
  val indexed = Signal[SourceIndex]()

  /** Requests that our project's code be fully reindexed. */
  def queueReindexAll () {
    project.pspace.indexQueue.tell(_ => reindexAll())
  }

  /** Requests that `store` be reindexed by this project's Codex. This requests that the code be
    * reindexed by the Codex and then that a [[SourceIndex]] be generated for the source. This will
    * be done in the background, and when the indexing is complete, a new `SourceIndex` instance
    * will be emitted via [[indexed]].
    */
  def queueReindex (store :Store) {
    // invoke the reindex in the background
    project.pspace.indexQueue.tell(_ => reindex(PSpaceCodex.toSource(store)))
  }

  /** Performs a full reindex of this project. */
  protected def reindexAll () {
    // by default we do nothing; WE HAVE NO INTELLIGENCE!
  }

  /** Performs the actual reindexing of `source`. This should call [[reindexComplete]] when the
    * indexing is complete. */
  protected def reindex (source :Source) {
    // by default we do nothing; WE HAVE NO INTELLIGENCE!
  }

  /** Called by subclasses to indicate that reindexing of a source file is complete. Reindexing
    * takes place on a background thread, and this method should also be called from the background
    * thread. */
  protected def reindexComplete (source :Source) {
    val ib = SourceIndex.builder(PSpaceCodex.toStore(source))
    if (project.store.visit(source, ib)) msvc.exec.runOnUI { indexed.emit(ib.build()) }
    else msvc.log.log(s"ProjectStore claims ignorance of just-indexed source? $source")
  }

  /** Returns a writer to use when indexing. */
  protected def writer :Writer = ???

  private def msvc = project.metaSvc
}
