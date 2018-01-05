//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.util.Close
import scaled.pacman.Config

object Generic {

  // TODO: unify all this into a single sectioned config file which we parse and cache and then
  // conditionally enable components based on the existence of appropriate sections; maybe even use
  // JSON, meh
  val LangFile = ".langserver"
  val MetaFile = ".scaled-project"

  class LangConfig (val config :Config) {
    val suffs = configSL("suff")
    val serverCmd = config.resolve("serverCmd", Config.StringP)
    val serverArgs = config.resolve("serverArg", Config.StringListP)
    config.finish()
    private def configS (name :String) = config.resolve(name, Config.StringP)
    private def configSL (name :String) = config.resolve(name, Config.StringListP)
  }

  class MetaConfig (config :Config) {
    val name = configS("name")
    val sourceDirs = configSL("sourceDir")
    val ignoreNames = configSL("ignoreName")
    val ignoreRegexes = configSL("ignoreRegex")
    config.finish()
    private def configS (name :String) = config.resolve(name, Config.StringP)
    private def configSL (name :String) = config.resolve(name, Config.StringListP)
  }

  def readConfig (path :Path) = new Config(Files.readAllLines(path))
  def readConfig (root :Path, name :String) :Config = readConfig(root.resolve(name))
  def readLangConfig (root :Path) :LangConfig = new LangConfig(readConfig(root, LangFile))
}

@Plugin(tag="langserver")
class GenericLangPlugin extends LangPlugin {
  import Generic._
  override def suffs (root :Path) = readLangConfig(root).suffs.toSet
  override def canActivate (root :Path) = Files.exists(root.resolve(LangFile))
  override def createClient (project :Project) = {
    val config = readLangConfig(project.root.path)
    Future.success(new LangClient(project, Seq(config.serverCmd) ++ config.serverArgs) {
      def name = "Generic"
    })
  }
}

@Plugin(tag="project-root")
class GenericRootPlugin extends RootPlugin.Directory(Generic.MetaFile)

@Plugin(tag="project-resolver")
class GenericResolverPlugin extends ResolverPlugin {
  import Generic._

  // // reinit if the config file changes
  // toClose += metaSvc.service[WatchService].watchFile(rootPath.resolve(MetaFile), file => reinit())

  def addComponents (project :Project) {
    val rootPath = project.root.path
    val metaFile = rootPath.resolve(MetaFile)
    if (Files.exists(metaFile)) {
      val config = new MetaConfig(readConfig(metaFile))

      val sb = Ignorer.stockIgnores
      config.ignoreNames.foreach { sb += Ignorer.ignoreName(_) }
      config.ignoreRegexes.foreach { sb += Ignorer.ignoreRegex(_) }
      project.addComponent(classOf[Filer], new DirectoryFiler(project, sb))

      val sourceDirs = config.sourceDirs.map(rootPath.resolve(_)).toSeq
      project.addComponent(classOf[Sources], new Sources(sourceDirs))

      val oldMeta = project.metaV()
      project.metaV() = oldMeta.copy(name = config.name)
    }
  }
}