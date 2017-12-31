//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

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

  private val userHome = Paths.get(System.getProperty("user.home"))

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

  private lazy val docfMap = {
    val map = MMap[String,DocFormatterPlugin]()
    val dfs = pluginSvc.resolvePlugins[DocFormatterPlugin]("doc-formatter")
    dfs.plugins.foreach { p => p.suffs.foreach { s => map.put(s, p) }}
    dfs.added.onValue { p => p.suffs.foreach { s => map.put(s, p) }}
    dfs.removed.onValue { p => p.suffs.foreach { s => map.remove(s) }}
    map
  }

  override def resolveByPaths (paths :List[Path]) :Project.Seed = {
    val (iseeds, rdseeds) = finderPlugins.flatMap(_.apply(paths)).partition(_.intelligent)
    val dseeds = filterDegenerate(rdseeds)
    // if there's exactly one intelligent project match, great!
    if (iseeds.size == 1) iseeds.head
    // if more than one intelligent matches: choose the deepest
    else if (iseeds.size > 1) {
      val deepestDepth = iseeds.map(_.root.path.getNameCount).max
      val diseeds = iseeds.filter(_.root.path.getNameCount == deepestDepth)
      if (diseeds.size == 1) diseeds.head
      else {
        // if there are >1 at the deepest depth, choose the project with highest prio, per prefTypes
        val prio = diseeds.minBy(s => config(prefTypes).indexOf(s.name) match {
          case -1 => Short.MaxValue
          case ii => ii
        })
        log.log(s"Multiple project matches in '${prio.root}'. " +
          s"Choosing ${prio.name} from ${diseeds.map(_.name)}).")
        prio
      }
    }
    // if there are any non-intelligent project matches, use the deepest match
    else if (!dseeds.isEmpty) dseeds.maxBy(_.root.path.getNameCount)
    // if all else fails, create a FileProject for the root
    else {
      val root = paths.head ; val file = root.getFileName.toString
      val clazz = if ((file endsWith ".zip") || (file endsWith ".jar")) classOf[ZipFileProject]
                  else classOf[FileProject]
      val proot = Project.Root(root, "")
      Project.Seed(proot, "file", false, clazz, List(proot))
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

  override def unknownProject (ps :ProjectSpace) =
    new Project(ps, Project.Root(Paths.get(""), "")) {
      val fileCompleter = Completer.file(ps.wspace.editor.exec)
      override def isIncidental = true
      override protected def computeMeta (oldMeta :Project.Meta) = Future.success(oldMeta)
    }

  override def didStartup () {
    // create our codex and stick it into editor state
    editor.state[Codex]() = new Codex(editor, metaSvc)

    // create a project space whenever a new workspace is opened (it will register itself in
    // workspace state and clear itself out when the workspace closes)
    editor.workspaceOpened.onValue { ws => new ProjectSpace(ws, metaSvc) }
  }

  override def willShutdown () {
    // shutdown our codex
    editor.state[Codex].get.close()
  }

  // filters out "degenerate" project seeds resolved by paths (i.e. the user's home directory, the
  // root of the file system)
  private def filterDegenerate (seeds :Seq[Project.Seed]) :Seq[Project.Seed] =
    seeds.filterNot { s => userHome.startsWith(s.root.path) || s.root.path.getParent == null}

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}
