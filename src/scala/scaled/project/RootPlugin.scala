//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Path, Files}
import scaled._

/** Used by the project system to identify project roots. Implementations must be tagged with
  * `@Plugin(tag="project-root")`.
  */
abstract class RootPlugin extends AbstractPlugin {

  /** Checks the supplied list of `paths` for a project root.
    * @return a root path if one is found, otherwise `None`.
    */
  def apply (paths :List[Path]) :Option[Project.Root] = {
    var best :Path = null ; var cur = paths
    while (!cur.isEmpty) {
      checkRoot(cur.head) match {
        case -1 => cur = if (best == null) cur.tail else Nil // stop if we have a root candidate
        case  0 => best = cur.head ; cur = cur.tail          // note current best candidate
        case  1 => best = cur.head ; cur = Nil               // stop the search
      }
    }
    Option(best).map(createRoot(paths, _))
  }

  /** Returns the root for the supplied project `id`, if known. */
  def apply (id :Project.Id) :Option[Project.Root] = None

  /** Checks whether `root` could be a root of a project.
    *
    * When identifying projects, the project manager searches up the directory hierarchy from some
    * arbitrary file (generally deep down inside the project), checking for roots in each directory
    * on the way up. It then chooses the most promising candidate as the root for a project (if any
    * candidates exist).
    *
    * The return value can be one of the following:
    * -  1 - directory is definitely a root, stop searching
    * -  0 - directory may be a root, note it and keep searching for higher root
    * - -1 - directory is not a root, if we have no root candidate thus far, keep searching, but
    *        otherwise stop searching and use our existing best candidate
    */
  protected def checkRoot (root :Path) :Int

  /** Creates a `Project.Root` for the supplied chosen best path.
    * @param paths the full list of paths searched, which may determine the root's module. For
    * example if we derived a project root for Maven project by starting from a test source file we
    * can create a root for the test module rather than the main module.
    */
  protected def createRoot (paths :List[Path], path :Path) = Project.Root(path)
}

object RootPlugin {

  /** Roots projects at directories that contain a specific named subdirectory. */
  class Directory (name :String) extends RootPlugin {
    def checkRoot (root :Path) = if (Files.isDirectory(root.resolve(name))) 1 else -1
  }

  /** Roots projects at directories that contain a specific named file. */
  class File (name :String) extends RootPlugin {
    def checkRoot (root :Path) = if (Files.exists(root.resolve(name))) 1 else -1
  }
}

/** Roots projects at .git directories. */
@Plugin(tag="project-root") class GitRootPlugin extends RootPlugin.Directory(".git")

/** Roots projects at .hg directories. */
@Plugin(tag="project-root") class MercurialRootPlugin extends RootPlugin.Directory(".hg")

/** Roots projects at the highest .svn directory. */
@Plugin(tag="project-root") class SubversionRootPlugin extends RootPlugin {
  def checkRoot (root :Path) = if (Files.isDirectory(root.resolve(".svn"))) 0 else -1
}
