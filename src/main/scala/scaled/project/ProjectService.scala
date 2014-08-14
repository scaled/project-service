//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** Provides miscellaneous project services. Chiefly project resolution, and some other global bits
  * and bobs. */
@Service(name="project", impl="ProjectManager", desc="Provides miscellaneous project services.")
trait ProjectService {

  def resolveByPaths (paths :List[Path]) :Project.Seed

  def resolveById (id :Project.Id) :Option[Project.Seed]

  def pathsFor (store :Store) :Option[List[Path]]

  def unknownProject (ps :ProjectSpace) :Project
}
