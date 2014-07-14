//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._
import scaled.util.Errors

/** Static [[ProjectSpace]] stuffs. */
object ProjectSpace {

  /** Resolves the project space for the supplied environment. */
  def apply (env :Env) :ProjectSpace = {
    val ws = env.editor.workspace ; val ps = ws.state[ProjectSpace]
    if (!ps.isDefined) ps() = new ProjectSpace(ws, env.msvc)
    ps.get
  }
}

/** Manages the projects in a workspace. */
class ProjectSpace (workspace :Workspace, val msvc :MetaService) extends AutoCloseable {
  import scala.collection.convert.WrapAsScala._
  import Project._

  workspace.toClose += this // our lifecycle matches that of our workspace
  private val pres = msvc.service[ResolverService]
  private def root = workspace.root
  private def log = msvc.log
  private val psdir = Files.createDirectories(root.resolve("Projects"))
  private val dsdir = Files.createDirectories(root.resolve("Depends"))

  // currently resolved projects
  private val projects = MMap[Path,Project]()
  // metadata on all projects added to this workspace; lazily resolved
  lazy private val (byId, toName) = {
    val byId = MMap[Id,Path]()
    val toName = MMap[Path,String]()
    Files.list(psdir).collect(Collectors.toList[Path]).foreach { dir =>
      if (Files.isDirectory(dir)) {
        val info = List() ++ Files.readAllLines(dir.resolve("info.txt"))
        val root = Paths.get(info.head)
        toName.put(root, dir.getFileName.toString)
        info.tail.flatMap(inflateId) foreach { id => byId.put(id, root) }
      }
    }
    (byId, toName)
  }

  /** Returns the name of the workspace. */
  def name :String = workspace.name

  /** Returns `(root, name)` for all projects in this workspace. */
  def allProjects :Seq[(Path,String)] = toName.toSeq

  /** Returns the project associated with `buffer`. */
  def project (buffer :RBuffer) :Project =
    buffer.state[Project].getOrElseUpdate(projectFor(buffer.store))

  /** Resolves (if necessary) and returns the project which handles `store`. */
  def projectFor (store :Store) :Project =
    pres.pathsFor(store).map(resolveByPaths).getOrElse(pres.unknownProject(this))

  /** Resolves (if necessary) and returns the project which is rooted at `root`. */
  def projectIn (root :Path) :Project = projectInRoot(root) getOrElse {
    throw Errors.feedback(s"No project in $root")
  }

  /** Resolves the project for `id`. */
  def projectFor (id :Id) :Option[Project] =
    byId.get(id).flatMap(projectInRoot).orElse(pres.resolveById(id).map(grow))

  /** Returns all currently resolved projects. */
  def loadedProjects :Seq[Project] = projects.values.toSeq

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = toName.get(proj.root) match {
    // if this is a named project in this workspace, use a dir based on its name
    case Some(name) => psdir.resolve(name)
    // if it's an incidental project (e.g. a random Maven dependency), use an id-based dir
    case None => dsdir.resolve(proj.idName)
  }

  /** Adds this project to this workspace. */
  def addProject (proj :Project) = {
    if (toName.contains(proj.root)) throw Errors.feedback(
      s"${proj.name} already added to this workspace.")

    // add this project to our ids map
    proj.ids.foreach { id => byId.put(id, proj.root) }

    // if this project's name is already in use by another project, tack -N onto it
    val names = toName.values.filter(_ startsWith proj.name).toSet
    val name = if (!names(proj.name)) proj.name else {
      var ext = 1
      while (names(proj.name + s"-$ext")) ext += 1
      proj.name + s"-$ext"
    }
    toName.put(proj.root, name)

    // move this project's metadir from Depends into Projects (or create it if it doesn't exist)
    val ddir = dsdir.resolve(proj.idName) ; val pdir = metaDir(proj)
    if (Files.exists(ddir)) Files.move(ddir, pdir)
    else Files.createDirectories(ddir)

    // write this project's id info its metadata dir
    updateInfo(proj)
  }
  // TODO: removeProject

  val codex :PSpaceCodex = new PSpaceCodex(this)
  override def close () {
    codex.close()
    // TODO: close/force-hibernate all resolved projects?
  }

  // the root passed here may have disappeared in the fullness of time, so validate it
  private def projectInRoot (root :Path) =
    if (root == null || !Files.exists(root)) None
    else projects.get(root) orElse Some(resolveByPaths(List(root)))

  private def resolveByPaths (paths :List[Path]) :Project = {
    val seed = pres.resolveByPaths(paths)
    projects.getOrElse(seed.root, grow(seed))
  }

  private def grow (seed :Project.Seed) = {
    val proj = msvc.injectInstance(seed.clazz, this :: seed.args)
    projects += (proj.root -> proj)
    // TODO: check project ids against byId and if any have changed, remap and updateInfo
    proj
  }

  private def updateInfo (proj :Project) {
    import scala.collection.convert.WrapAsJava._
    val ids = metaDir(proj).resolve("info.txt")
    Files.write(ids, List(proj.root.toString) ++ proj.ids.map(_.deflate))
  }
}
