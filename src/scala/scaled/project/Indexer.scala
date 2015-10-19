//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.{Extractor, SourceSet, TextWriter, Writer}
import codex.model.Source
import java.nio.file.{Path, Paths}
import scaled._
import scaled.util.Errors

/** Handles indexing source files, for Codex. */
class Indexer (val pspace :ProjectSpace) {
  import pspace.msvc

  /** A signal emitted when a source has been reprocessed and indexed. */
  val indexed = Signal[SourceIndex]()

  /** An execution queue on which Codex indexing is run. This serializes all indexing actions which
    * avoids grinding a user's machine to a halt with multiple full indexes. */
  val indexQueue :Pipe[Unit] = msvc.process(())

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
    indexQueue.tell(_ => reindex(project, PSpaceCodex.toSource(store), force))
  }

  /** Performs a debug reindex of store, writing the output to stdout. */
  def debugReindex (project :Project, store :Store) {
    if (System.console == null) throw Errors.feedback("No console, can't emit debug output.")
    val source = PSpaceCodex.toSource(store)
    extractor(project, source.fileExt) foreach { ex =>
      ex.process(SourceSet.create(source), new TextWriter(System.console.writer))
    }
  }

  /** Performs a full reindex of this project. This method is called on a background thread. */
  protected def reindexAll (project :Project) {
    val sums = project.summarizeSources
    project.store.clear()
    sums foreach { (suff, srcs) =>
      extractor(project, suff) foreach { ex =>
        project.pspace.wspace.statusMsg.emit(
          s"Reindexing ${srcs.size} $suff files in ${project.name}...")
        ex.process(srcs, project.store.writer)
        project.pspace.wspace.statusMsg.emit(s"Reindex of ${project.name} $suff files complete.")
      }
    }
  }

  /** Performs the actual reindexing of `source`. This method is called on a background thread. */
  protected def reindex (project :Project, source :Source, force :Boolean) {
    if (force || source.lastModified > project.store.lastIndexed(source)) {
      extractor(project, source.fileExt) foreach { ex =>
        println(s"Reindexing: $source")
        ex.process(SourceSet.create(source), project.store.writer)
      }
    } // else println(s"Source up to date: $source")
    reindexComplete(project, source)
  }

  /** Called when reindexing of a source file is complete. Reindexing takes place on a background
    * thread, and this method is called therefrom. */
  protected def reindexComplete (project :Project, source :Source) {
    val ib = SourceIndex.builder(PSpaceCodex.toStore(source))
    if (project.store.visit(source, ib)) msvc.exec.runOnUI { indexed.emit(ib.build()) }
    // TODO: until we know what file extensions are known to the indexer, this generates too many
    // spurious warnings to be useful
    // else msvc.log.log(s"ProjectStore claims ignorance of just-indexed source? $source")
  }

  private def extractor (project :Project, suff :String) :Option[Extractor] = {
    val iter = extractorPlugins.plugins.filter(_.suffs.contains(suff)).iterator()
    while (iter.hasNext()) { // blah
      val exo = iter.next.extractor(project, suff)
      if (exo.isDefined) return exo
    }
    None
  }
  private lazy val extractorPlugins = {
    val set = msvc.service[PluginService].resolvePlugins[ExtractorPlugin]("codex-extractor")
    pspace.wspace.toClose += set
    set
  }
}
