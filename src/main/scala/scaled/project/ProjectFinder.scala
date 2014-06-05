//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled.AbstractPlugin
import scaled.MetaService

/** Finders are used to identify projects given only the location of a file somewhere in the bowels
  * of the project. This is generally done by searching up the directory hierarchy, looking for
  * sentinel files that indicate that a particular kind of project is in effect.
  *
  * ProjectFinderPlugin implementations must be tagged with `@Plugin("project-finder")`.
  *
  * @param name a `foo-bar` style name that is used to allow the user to configure custom
  * priorities for project finders if they don't like the defaults. Examples: `git`, `maven`,
  * `sbt`, `makefile`.
  * @param intelligent indicates that this finder provides projects with real intelligence (which
  * grok the build system that identifies the project). Such projects will be chosen in favor of
  * non-intelligent projects which do little more than enumerate all files in a project and make
  * do from there. In the unlikely event that multiple intelligent projects match the same directory
  * structure, one will be chosen arbitrarily and a warning will be issued. The user can then
  * override the chosen project type, if desired, in the `.scaled/config.properties` file placed
  * in the project root.
  * @param projectClass the class that implements projects identified by this finder. This class
  * will be instantiated using the Scaled dependency injection mechanism, with a single `Path`
  * argument (the root of the project). Using the dependency injection mechanism allows the project
  * to inject any Scaled services it may need.
  */
abstract class ProjectFinderPlugin (
  val name :String, val intelligent :Boolean, projectClass :Class[_ <: Project]
) extends AbstractPlugin {

  /** Checks whether `root` could be a root of a project of the type sought by this finder.
    *
    * When identifying projects, the project manager searches up the directory hierarchy from
    * some arbitrary file (generally deep down inside the project), checking for roots in each
    * directory on the way up. It then chooses the most promising candidate as the root for a
    * project (if any candidates exist).
    *
    * @return -1 if this directory is definitely not a root, 1 if this directory is definitely a
    * root and the search should stop here, or 0 if this directory could be a root, but so could
    * a directory higher up the hierarchy. In the absence of a 1 directory, the algorithm chooses
    * the 0 directory nearest to the file-system root.
    */
  def checkRoot (root :Path) :Int

  /** Applies this finder to the supplied path list.
    * @return `Some(root,intelligent,thunk)` if it matches, otherwise `None`.
    */
  def apply (paths :List[Path]) :Option[(Path,Boolean,MetaService => Project)] = {
    var best :Path = null ; var cur = paths
    while (!cur.isEmpty) {
      checkRoot(cur.head) match {
        case -1 => /* skip it */     cur = cur.tail
        case  0 => best = cur.head ; cur = cur.tail // note current best candidate
        case  1 => best = cur.head ; cur = Nil      // stop the search
      }
    }
    if (best == null) None else Some((best, intelligent, createProjectIn(best)))
  }

  /** Applies this finder to the supplied id.
    * @return `Some(thunk)` if this project finder knows how to provide a project for `id`, `None`
    * otherwise.
    */
  def apply (id :Project.Id) :Option[MetaService => Project] = None

  /** Returns a thunk that will create our project, which we identified as being rooted at `root`.
    * The default passes the root as an injection argument (and nothing else). */
  protected def createProjectIn (root :Path) = createProject(root)

  /** Returns a thunk that will inject our project with the supplied arguments. */
  protected def createProject (args :Any*) = (metaSvc :MetaService) => {
    metaSvc.injectInstance(projectClass, args.toList)
  }

  protected def exists (dir :Path, file :String) :Boolean = Files.exists(dir.resolve(file))
}
