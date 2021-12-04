//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.HashMap
import java.util.stream.Collectors
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._
import scaled.major.TextConfig
import scaled.util.{BufferBuilder, Describable, Errors, Close}

/** Manages the projects in a workspace. */
class ProjectSpace (val wspace :Workspace, val msvc :MetaService)
    extends AutoCloseable with Describable {
  import Project._

  wspace.state[ProjectSpace]() = this
  wspace.toClose += this // our lifecycle matches that of our workspace

  // when a buffer is opened, resolve the project for the path edited by the buffer and add it
  wspace.toClose += wspace.bufferOpened.onValue { buf =>
    // defer this resolution until the next UI tick to avoid collision if a failure in project
    // resolution tries to create a new buffer while we're still processing the 'buffer opened'
    // signal
    msvc.exec.runOnUI({
      if (!buf.state[Project].isDefined)
        psvc.pathsFor(buf.store).map(resolveByPaths).ifDefined(_.addToBuffer(buf))
    })
  }

  private val codex = Codex(wspace.editor)
  private val psvc = msvc.service[ProjectService]
  private def root = wspace.root
  private val toClose = Close.bag()

  private lazy val resolvers = msvc.service[PluginService].
    resolvePlugins[ResolverPlugin]("project-resolver")

  // metadata for all named projects
  lazy private val pdb = new ProjectDB(msvc.exec, root, log)

  // currently resolved projects
  private val projects = new HashMap[Root,Project]()

  /** Returns the name of the workspace. */
  def name :String = wspace.name

  /** Easy access to our logger. */
  def log = msvc.log

  /** Returns `(root, name)` for all projects in this workspace. */
  def allProjects :Seq[(Root,String)] = pdb.toInfo.values.map(_.rootName).toSeq

  /** Resolves a project in the supplied `root`. */
  def projectFor (root :Root) = Option(projects.get(root)) || {
    val proj = new Project(this, root)
    projects.put(root, proj)
    // when the project's metadata changes...
    proj.metaV.onValueNotify { _ =>
      // make sure the project is properly mapped in the project DB
      pdb.checkInfo(proj)
      // let the codex know that it might want to index the project
      codex.checkProject(proj)
    }
    // let the resolvers add components to the project
    resolvers.plugins.foreach { plugin =>
      val metaFiles = plugin.metaFiles(proj.root)
      if (metaFiles.isEmpty) plugin.addComponents(proj)
      else {
        // if the plugin reports meta files, watch those and readd the plugin on change
        val exists = metaFiles.filter(file => Files.exists(file))
        if (!exists.isEmpty) {
          plugin.addComponents(proj)
          val watchSvc = msvc.service[WatchService]
          exists.foreach { path =>
            proj.toClose += watchSvc.watchFile(path, _ => plugin.readdComponents(proj))
          }
        }
      }
    }
    proj
  }

  /** Resolves the project for `id`. */
  def projectFor (id :Id) :Option[Project] =
    knownProjectFor(id) orElse psvc.resolveById(id).map(projectFor)

  /** Resolves the project for `id` if that project is registered our project database. Unlike
    * [[projectFor]] this will not resolve unregistered projects (usually depends). */
  def knownProjectFor (id :Id) :Option[Project] = id match {
    case RootId(path, module) => Some(projectFor(Root(path, module)))
    case _                    => Option(pdb.byId.get(id)).map(projectFor)
  }

  /** Returns all currently resolved projects. */
  def loadedProjects :Seq[Project] = projects.values.toSeq

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = pdb.metaDir(proj)

  /** The history ring for execution invocations. */
  val execHistory = new Ring(32)

  /** Adds `proj` to this workspace. */
  def addProject (proj :Project) :Unit = {
    // add the project to our database
    if (!pdb.add(proj)) throw Errors.feedback(s"${proj.name} already added to this workspace.")
    // add this project's root to our workspace's hint path
    wspace.addHintPath(proj.root.path)
  }

  /** Removes `proj` from this workspace. */
  def removeProject (proj: Project) :Unit = {
    // remove this project's root from our workspace's hint path
    wspace.removeHintPath(proj.root.path)
    // remove the project from our database
    if (!pdb.remove(proj)) throw Errors.feedback(s"${proj.name} not added to this workspace.")
  }

  /** Manages executions for this project space. */
  lazy val execs :Executions = new Executions(this)

  private lazy val langPlugins = msvc.service[PluginService].
    resolvePlugins[LangPlugin]("langserver")
  private def langPluginFor (suff :String, root :Project.Root) :Option[LangPlugin] =
    langPlugins.plugins.filter(_.canActivate(root)).find(_.suffs(root).contains(suff))

  private case class LangKey (suff :String, root :Path, module :Option[String])
  private val langClients = new HashMap[LangKey,Future[LangClient]]()

  /** Obtains a `LangClient` for the specified `project` and source code file `suff` (i.e. `java`,
    * `scala`, etc.). If an applicable client is already available, it is used, otherwise if a
    * plugin can be found that can start a new language server a new server is started and then
    * client to it returned. */
  def langClientFor (project :Project, suff :String) :Option[Future[LangClient]] = {
    val root = project.root
    val genKey = LangKey(suff, root.path, None)
    val modKey = LangKey(suff, root.path, Some(root.module))
    def langClient (key :LangKey) = Option(langClients.get(key))
    langClient(genKey) orElse langClient(modKey) orElse langPluginFor(suff, root).flatMap(
      plugin => try {
        val client = plugin.createClient(project)
        val key = if (plugin.moduleSpecific) modKey else genKey
        plugin.suffs(root).foreach { suff => langClients.put(key, client) }
        // TODO: close lang clients if all buffers with their suff are closed
        client onSuccess { client => {
          toClose += client
          project.toClose += client.messages.onValue(project.emitStatus(_))
        }}
        client onFailure project.exec.handleError
        Some(client)
      } catch {
        case t :Throwable => project.exec.handleError(t) ; None
      })
  }

  /** Closes any active language client for `project` and `suff`. A subsequent call to
    * `langClientFor` will re-resolve a new client. */
  def closeLangClientFor (project :Project, suff :String) :Unit = {
    val root = project.root
    def close (clientF :Future[LangClient]) :Unit = {
      if (clientF != null) clientF onSuccess { _.close() }
    }
    close(langClients.remove(LangKey(suff, root.path, None)));
    close(langClients.remove(LangKey(suff, root.path, Some(root.module))));
  }

  override def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addHeader(s"Projects")
    val allps = allProjects
    if (allps.isEmpty) bb.add("<none>")
    else {
      val width = (0 /: allps)((m, p) => math.max(m, p._2.length))
      def pad (name :String) = name + (" " * (width-name.length))
      allps.sortBy(_._2) foreach { case (root, name) =>
        val key = pad(name)
        val lb = Line.builder(s"$key ${root.toString}").
          withLineTag(Visit.Tag(new Visit() {
            protected def go (window :Window) = projectFor(root).visitDescription(window)
          })).
          withStyle(TextConfig.prefixStyle, 0, key.length)
        bb.add(lb.build())
      }
    }

    bb.addSubHeader("Loaded Projects")
    for (p <- loadedProjects) {
      bb.addSection(Line.builder(p.name).withLineTag(Visit.Tag(new Visit() {
        protected def go (window :Window) = p.visitDescription(window)
      })))
      bb.addKeysValues("Kind: " -> p.getClass.getName,
                       "Root: " -> p.root.toString(),
                       "Ids: "  -> p.ids.mkString(" "),
                       "Deps: " -> p.depends.ids.size.toString)
    }

    if (!langClients.isEmpty) {
      bb.addSubHeader("Lang Clients")
      // ugly hack to provide info for already resolved clients
      def info (client :Future[LangClient]) :String = {
        var info = "<resolving>"
        client.onSuccess(client => info = client.name)
        info
      }
      bb.addKeysValues(langClients.entrySet.map(ent => (s"${ent.getKey}: ", info(ent.getValue))))
    }

    execs.describeSelf(bb)
  }

  override def close () :Unit = {
    toClose.close()
    projects.values.foreach(_.dispose())
    wspace.state[ProjectSpace].clear()
  }

  private def resolveByPaths (paths :List[Path]) :Project = projectFor(psvc.resolveByPaths(paths))
}

/** Static helpers. */
object ProjectSpace {

  /** Returns the `ProjectSpace` associated with `wspace`. */
  def apply (wspace :Workspace) :ProjectSpace = wspace.state[ProjectSpace].getOrElse {
    throw new IllegalStateException(s"No ProjectSpace configured in workspace: '$wspace'")
  }
}
