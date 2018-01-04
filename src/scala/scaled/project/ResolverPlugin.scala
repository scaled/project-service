//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._

/** Used to resolve projects. When Scaled is opened with some arbitrary file in some arbitrary
  * directory, if no project is already resolved that contains the opened file, a new project is
  * resolved. The root of the new project is deterimed by the `RootPlugin` set. A project is then
  * created at that root and the resolver plugins are given a chance to add components to it.
  *
  * Implementations must be tagged with `@Plugin("project-resolver")`.
  */
abstract class ResolverPlugin extends AbstractPlugin {

  /** Adds any components to a just-resolved project that this resolver can provide. The project's
    * `root` will be established, so the resolver can check in that root for sentinel files that
    * indicate that it should provide its services. (Like a `pom.xml` or the files for some other
    * build or project management system.)
    */
  def addComponents (project :Project) :Unit
}
