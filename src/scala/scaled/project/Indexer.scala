//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.{Extractor, TextWriter, Writer}
import codex.model.Source
import java.nio.file.{Path, Paths}
import scaled._

/** Handles indexing source files, for Codex. */
class Indexer (val project :Project) {
  import project.{metaSvc => msvc}

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
    * @param force if true, file is indexed regardless of whether its last modified time is more
    * recent than the file's last recorded index.
    */
  def queueReindex (store :Store, force :Boolean) {
    // invoke the reindex in the background
    project.pspace.indexQueue.tell(_ => reindex(PSpaceCodex.toSource(store), force))
  }

  /** Performs a debug reindex of store, writing the output to stdout. */
  def debugReindex (store :Store) {
    val source = PSpaceCodex.toSource(store)
    val path = Paths.get(source.toString)
    extractor(source.fileExt, isTestSource(path)) foreach { ex =>
      ex.process(path, new TextWriter(System.console.writer))
    }
  }

  /** Performs a full reindex of this project. This method is called on a background thread. */
  protected def reindexAll () {
    val sums = project.summarizeSources(false)
    project.store.clear()
    sums.asMap.toMapV foreach { (suff, srcs) =>
      extractor(suff, false) foreach { ex =>
        project.pspace.wspace.statusMsg.emit(
          s"Reindexing ${srcs.size} $suff files in ${project.name}...")
        ex.process(srcs, project.store.writer)
      }
    }
  }

  /** Performs the actual reindexing of `source`. This method is called on a background thread. */
  protected def reindex (source :Source, force :Boolean) {
    val path = Paths.get(source.toString)
    if (!isTestSource(path)) { // TODO: handle test sources when we have a separate test store
      if (force || source.lastModified > project.store.lastIndexed(source)) {
        extractor(source.fileExt, false) foreach { ex =>
          println(s"Reindexing: $source")
          ex.process(path, project.store.writer)
        }
      } // else println(s"Source up to date: $source")
    } else println(s"TODO: reindex test source: $source")
    reindexComplete(source)
  }

  private def isTestSource (path :Path) = project.testSourceDirs exists(d => path startsWith d)

  /** Called by subclasses to indicate that reindexing of a source file is complete. Reindexing
    * takes place on a background thread, and this method may be called therefrom. */
  protected def reindexComplete (source :Source) {
    val ib = SourceIndex.builder(PSpaceCodex.toStore(source))
    if (project.store.visit(source, ib)) msvc.exec.runOnUI { indexed.emit(ib.build()) }
    // TODO: until we know what file extensions are known to the indexer, this generates too many
    // spurious warnings to be useful
    // else msvc.log.log(s"ProjectStore claims ignorance of just-indexed source? $source")
  }

  /** Returns an extractor to use when indexing.
    * @param suff the suffix of the file being indexed (`java`, `scala`, etc.).
    * @param forTest whether the file to be indexed is a main or test source file. */
  protected def extractor (suff :String, forTest :Boolean) :Option[Extractor] = None
}
