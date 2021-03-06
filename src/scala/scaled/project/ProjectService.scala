//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** Provides miscellaneous project services.
  * Chiefly project resolution, and some other global bits and bobs. */
@Service(name="project", impl="ProjectManager", autoLoad=true,
         desc="Provides miscellaneous project services.")
trait ProjectService {

  def resolveByPaths (paths :List[Path]) :Project.Root

  def resolveById (id :Project.Id) :Option[Project.Root]

  def pathsFor (store :Store) :Option[List[Path]]

  def unknownProject (ps :ProjectSpace) :Project

  /** Returns the doc formatter for the specified source language (identified by file suffix). */
  def docFormatter (lang :String) :DocFormatterPlugin
}
