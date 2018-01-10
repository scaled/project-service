//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** Used to resolve projects. When Scaled is opened with some arbitrary file in some arbitrary
  * directory, if no project is already resolved that contains the opened file, a new project is
  * resolved. The root of the new project is deterimed by the `RootPlugin` set. A project is then
  * created at that root and the resolver plugins are given a chance to add components to it.
  *
  * Implementations must be tagged with `@Plugin("project-resolver")`.
  */
abstract class ResolverPlugin extends AbstractPlugin {

  /** Returns the meta files used by this resolver plugin based on the supplied project `root`.
    * If any of the returned files exist, this resolver plugin will be applied to the project, and
    * the files will be watched such that if any of them change, the resolver will be reapplied to
    * the project. If no files are returned the resolver plugin will be applied to every project
    * but with no automatic reloading.
    */
  def metaFiles (root :Project.Root) :Seq[Path] = Seq()

  /** Adds any components to a just-resolved project that this resolver can provide. The project's
    * `root` will be established, so the resolver can check in that root for sentinel files that
    * indicate that it should provide its services. (Like a `pom.xml` or the files for some other
    * build or project management system.)
    */
  def addComponents (project :Project) :Unit

  /** Called when one of this plugin's meta files has changed. The default implementation calls
    * [[addComponents]] again because simply reparsing the config file and adding the same set of
    * components will replace old components with new components which is what most plugins will
    * want. This method exists for the rare plugin that needs to do something special on reload
    * versus initial load.
    */
  def readdComponents (project :Project) :Unit = addComponents(project)
}
