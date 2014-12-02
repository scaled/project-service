//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._

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
  * @param clazz the class that implements projects identified by this finder. This class will be
  * instantiated using the Scaled dependency injection mechanism, with a single `Path` argument (the
  * root of the project). Using the dependency injection mechanism allows the project to inject any
  * Scaled services it may need.
  */
abstract class ProjectFinderPlugin (
  val name :String, intelligent :Boolean, clazz :Class[_ <: Project]
) extends AbstractPlugin {

  /** Checks whether `root` could be a root of a project of the type sought by this finder.
    *
    * When identifying projects, the project manager searches up the directory hierarchy from
    * some arbitrary file (generally deep down inside the project), checking for roots in each
    * directory on the way up. It then chooses the most promising candidate as the root for a
    * project (if any candidates exist).
    *
    * The return value can be one of the following:
    * -  1 - directory is definitely a root, stop searching
    * -  0 - directory may be a root, note it and keep searching for higher root
    * - -1 - directory is not a root, if we have no root candidate thus far, keep searching, but
    *        otherwise stop searching and use our existing best candidate
    */
  def checkRoot (root :Path) :Int

  /** Applies this finder to the supplied path list.
    * @return a project seed if a match is found, otherwise `None`.
    */
  def apply (paths :List[Path]) :Option[Project.Seed] = {
    var best :Path = null ; var cur = paths
    while (!cur.isEmpty) {
      checkRoot(cur.head) match {
        case -1 => cur = if (best == null) cur.tail else Nil // stop if we have a root candidate
        case  0 => best = cur.head ; cur = cur.tail // note current best candidate
        case  1 => best = cur.head ; cur = Nil      // stop the search
      }
    }
    if (best == null) None else Some(seed(best, injectArgs(paths.head, best)))
  }

  /** Applies this finder to the supplied id.
    * @return a project seed if this project finder knows how to provide a project for `id`, `None`
    * otherwise.
    */
  def apply (id :Project.Id) :Option[Project.Seed] = None

  /** Returns any additional injection args for a project created in `root`. */
  protected def injectArgs (top :Path, root :Path) :List[Any] = root :: Nil

  /** Returns a seed for a project in `root`, with injection args `args`. */
  protected def seed (root :Path, args :List[Any]) =
    Project.Seed(root, name, intelligent, clazz, args)

  /** Returns true if `dir/file` exists. Helpy helper! */
  protected def exists (dir :Path, file :String) :Boolean = Files.exists(dir.resolve(file))
}
