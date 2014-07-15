//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scaled._

/** A service for resolving projects. Is a service solely so we have a JVM-wide singleton. */
@Service(name="resolver", impl="ResolverService", desc="Handles resolution of projects.")
class ResolverService (metaSvc :MetaService) extends AbstractService {

  private val pluginSvc = metaSvc.service[PluginService]
  private def log = metaSvc.log

  private val finders = pluginSvc.resolvePlugins[ProjectFinderPlugin]("project-finder")
  // a special resolver for our config file directory
  private val configFinder = new FileProject.FinderPlugin("scaled-config") {
    val configRoot = metaSvc.metaFile("Config")
    def checkRoot (root :Path) = if (root == configRoot) 1 else -1
  }
  private def finderPlugins = finders.plugins :+ configFinder

  def resolveByPaths (paths :List[Path]) :Project.Seed = {
    val (iprojs, dprojs) = finderPlugins.flatMap(_.apply(paths)).partition(_.intelligent)
    // if there are more than one intelligent project matches, complain
    if (!iprojs.isEmpty) {
      if (iprojs.size > 1) {
        log.log(s"Multiple intelligent project matches:")
        iprojs.foreach { p => log.log(s"  $p") }
      }
      iprojs.head
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dprojs.isEmpty) dprojs.maxBy(_.root.getNameCount)
    // if all else fails, create a FileProject for the root
    else {
      val root = paths.last ; val file = root.getFileName.toString
      val clazz = if ((file endsWith ".zip") || (file endsWith ".jar")) classOf[ZipFileProject]
                  else classOf[FileProject]
      Project.Seed(root, false, clazz, List(root))
    }
  }

  def resolveById (id :Project.Id) :Option[Project.Seed] = {
    val iter = finderPlugins.iterator
    while (iter.hasNext) {
      val seed = iter.next.apply(id)
      if (seed.isDefined) return seed
    }
    None
  }

  def pathsFor (store :Store) :Option[List[Path]] = store match {
    case FileStore(path)       => Some(parents(path.getParent))
    case ZipEntryStore(zip, _) => Some(List(zip))
    case _                     => None
  }

  def unknownProject (ps :ProjectSpace) = new Project(ps) {
    val fileCompleter = Completer.file
    override val root = Paths.get("")
    override def isIncidental = true
    override def name = "<unknown>"
    override def idName = "unknown"
  }

  override def didStartup () {}
  override def willShutdown () {}

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}
