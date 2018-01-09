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
  import Project._

  private val userHome = Paths.get(System.getProperty("user.home"))

  private def pluginSvc = metaSvc.service[PluginService]
  private def log = metaSvc.log
  private val config = metaSvc.service[ConfigService].
    resolveServiceConfig("project", ProjectServiceConfig :: Nil)

  private lazy val rootPluginSet = pluginSvc.resolvePlugins[RootPlugin]("project-root")
  // a special root plugin for our config file directory
  private val configRoot = new RootPlugin {
    val configRoot = metaSvc.metaFile("Config")
    def checkRoot (root :Path) = if (root == configRoot) 1 else -1
  }
  private def rootPlugins = rootPluginSet.plugins :+ configRoot

  private lazy val docfMap = {
    val map = MMap[String,DocFormatterPlugin]()
    val dfs = pluginSvc.resolvePlugins[DocFormatterPlugin]("doc-formatter")
    dfs.plugins.foreach { p => p.suffs.foreach { s => map.put(s, p) }}
    dfs.added.onValue { p => p.suffs.foreach { s => map.put(s, p) }}
    dfs.removed.onValue { p => p.suffs.foreach { s => map.remove(s) }}
    map
  }

  override def resolveByPaths (paths :List[Path]) :Root = {
    val viablePaths = filterDegenerate(paths)
    rootPlugins.flatMap(_(viablePaths)) match {
      case Seq() =>
        log.log(s"Unable to find project root, falling back to ${paths.head}")
        Root(paths.head)
      case Seq(root) => root
      case roots =>
        val byDepth = roots.sortBy(-_.path.getNameCount)
        log.log(s"Using deepest of multiple project roots: $byDepth")
        byDepth.head
    }
  }

  override def resolveById (id :Id) :Option[Root] = id match {
    case RootId(path, module) => Some(Root(path, module))
    case _ => {
        val iter = rootPlugins.iterator
        while (iter.hasNext) {
          val root = iter.next.apply(id)
          if (root.isDefined) return root
        }
        None
      }
  }

  override def pathsFor (store :Store) :Option[List[Path]] = store match {
    case FileStore(path)       => Some(parents(path.getParent))
    case ZipEntryStore(zip, _) => Some(List(zip))
    case _                     => None
  }

  override def docFormatter (suff :String) = docfMap.getOrElse(suff, DocFormatterPlugin.Default)

  override def unknownProject (ps :ProjectSpace) = new Project(ps, Root(Paths.get(""), "")) {
    override def isIncidental = true
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
  private def filterDegenerate (paths :List[Path]) :List[Path] =
    paths.filterNot { p => userHome.startsWith(p) || p.getParent == null}

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}
