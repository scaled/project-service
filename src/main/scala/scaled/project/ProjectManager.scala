//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scaled._

/** Configuration for [[ProjectService]]. */
object ProjectServiceConfig extends Config.Defs {

  @Var("""The order of preference for project types. Used when multiple project types match
          a given project on the file system.""")
  val prefTypes = key(Seq("scaled", "maven"))
}

class ProjectManager (metaSvc :MetaService, editor :Editor)
    extends AbstractService with ProjectService {
  import ProjectServiceConfig._

  private def pluginSvc = metaSvc.service[PluginService]
  private def log = metaSvc.log
  private val config = metaSvc.service[ConfigService].resolveServiceConfig(
    "project", ProjectServiceConfig :: Nil)

  private lazy val finders = pluginSvc.resolvePlugins[ProjectFinderPlugin]("project-finder")
  // a special resolver for our config file directory
  private val configFinder = new FileProject.FinderPlugin("scaled-config") {
    val configRoot = metaSvc.metaFile("Config")
    def checkRoot (root :Path) = if (root == configRoot) 1 else -1
  }
  private def finderPlugins = finders.plugins :+ configFinder

  private val docfMap = MMap[String,DocFormatterPlugin]()
  private lazy val docfSet = {
    val dfs = pluginSvc.resolvePlugins[DocFormatterPlugin]("doc-formatter")
    dfs.plugins.foreach { p => p.suffs.foreach { s => docfMap.put(s, p) }}
    dfs.added.onValue { p => p.suffs.foreach { s => docfMap.put(s, p) }}
    dfs.removed.onValue { p => p.suffs.foreach { s => docfMap.remove(s) }}
    dfs
  }

  // create a project space whenever a new workspace is opened (it will register itself in
  // workspace state and clear itself out when the workspace hibernates)
  editor.workspaceOpened.onValue { ws => new ProjectSpace(ws, metaSvc) }

  override def resolveByPaths (paths :List[Path]) :Project.Seed = {
    val (iseeds, dseeds) = finderPlugins.flatMap(_.apply(paths)).partition(_.intelligent)
    // if there's exactly one intelligent project match, great!
    if (iseeds.size == 1) iseeds.head
    // if more than one intelligent matches: choose the project with highest prio, per prefTypes
    else if (iseeds.size > 1) {
      val prio = iseeds.minBy(s => config(prefTypes).indexOf(s.name) match {
        case -1 => Short.MaxValue
        case ii => ii
      })
      log.log(s"Multiple project matches in '${prio.root}'. " +
              s"Choosing ${prio.name} from ${iseeds.map(_.name)}).")
      prio
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dseeds.isEmpty) dseeds.maxBy(_.root.getNameCount)
    // if all else fails, create a FileProject for the root
    else {
      val root = paths.last ; val file = root.getFileName.toString
      val clazz = if ((file endsWith ".zip") || (file endsWith ".jar")) classOf[ZipFileProject]
                  else classOf[FileProject]
      Project.Seed(root, "file", false, clazz, List(root))
    }
  }

  override def resolveById (id :Project.Id) :Option[Project.Seed] = {
    val iter = finderPlugins.iterator
    while (iter.hasNext) {
      val seed = iter.next.apply(id)
      if (seed.isDefined) return seed
    }
    None
  }

  override def pathsFor (store :Store) :Option[List[Path]] = store match {
    case FileStore(path)       => Some(parents(path.getParent))
    case ZipEntryStore(zip, _) => Some(List(zip))
    case _                     => None
  }

  override def docFormatter (suff :String) = docfMap.getOrElse(suff, DocFormatterPlugin.Default)

  override def unknownProject (ps :ProjectSpace) = new Project(ps) {
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
