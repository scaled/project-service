//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import org.eclipse.lsp4j._
import scaled._
import scaled.pacman.Config
import scaled.util.Close

object GenericLangProject {

  val ProjectFile = ".lsproject"

  class GPConfig (val config :Config) {
    val name = configS("name")
    val sourceDirs = configSL("sourceDir")
    val ignoreNames = configSL("ignoreName")
    val ignoreRegexes = configSL("ignoreRegex")
    val suffs = configSL("suff")
    val serverCmd = config.resolve("serverCmd", Config.StringP)
    val serverArgs = config.resolve("serverArg", Config.StringListP)
    config.finish()
    private def configS (name :String) = config.resolve(name, Config.StringP)
    private def configSL (name :String) = config.resolve(name, Config.StringListP)
  }
  def readConfig (root :Path) :GPConfig =
    new GPConfig(new Config(Files.readAllLines(root.resolve(ProjectFile))))

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("langserver", true, classOf[GenericLangProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, ProjectFile)) 1 else -1
  }

  @Plugin(tag="langserver")
  class GenericLangPlugin extends LangPlugin {
    override def suffs (root :Path) = readConfig(root).suffs.toSet
    override def canActivate (root :Path) = Files.exists(root.resolve(ProjectFile))
    override def createClient (project :Project) = {
      val config = readConfig(project.root.path)
      Future.success(new LangClient(project, Seq(config.serverCmd) ++ config.serverArgs) {
        def name = "Generic"
      })
    }
  }
}

class GenericLangProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  import GenericLangProject._

  // reinit if the config file changes
  toClose += metaSvc.service[WatchService].watchFile(configFile, file => reinit())

  override protected def computeMeta (oldMeta :Project.Meta) = try {
    val sb = FileProject.stockIgnores
    config.get.ignoreNames.foreach { sb += FileProject.ignoreName(_) }
    config.get.ignoreRegexes.foreach { sb += FileProject.ignoreRegex(_) }
    ignores() = sb

    addComponent(classOf[Compiler], new LangCompiler(this))

    Future.success(oldMeta.copy(
      name = config.get.name,
      sourceDirs = config.get.sourceDirs.map(rootPath.resolve(_)).toSeq
    ))
  } catch {
    case err :Throwable => Future.failure(err)
  }

  // use our ignores when enumerating sources
  override def onSources (op :Path => Unit) :Unit = onFiles(sourceDirs, op)

  private def onFiles (dirs :SeqV[Path], op :Path => Unit) :Unit = {
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.{SimpleFileVisitor, FileVisitResult}
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op(file)
          FileVisitResult.CONTINUE
        }
        override def preVisitDirectory (dir :Path, attrs :BasicFileAttributes) = {
          if (ignore(dir)) FileVisitResult.SKIP_SUBTREE
          else FileVisitResult.CONTINUE
        }
      })
    }
  }

  private[this] val config = new Close.Ref[GPConfig](toClose) {
    protected def create = readConfig(rootPath)
  }
  private def rootPath = root.path
  private def configFile = rootPath.resolve(ProjectFile)
}
