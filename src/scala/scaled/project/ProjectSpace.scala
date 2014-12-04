//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.HashMap
import java.util.stream.Collectors
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Errors}

/** Manages the projects in a workspace. */
class ProjectSpace (val wspace :Workspace, val msvc :MetaService) extends AutoCloseable {
  import Project._

  wspace.state[ProjectSpace]() = this
  wspace.toClose += this // our lifecycle matches that of our workspace

  // when a buffer is opened, stuff its project and related bits into buffer state
  wspace.toClose += wspace.bufferOpened.onValue { buf =>
    val pstate = buf.state[Project]
    if (!pstate.isDefined) psvc.pathsFor(buf.store).map(resolveByPaths) map { proj =>
      pstate.update(proj)
      import Config.Scope
      buf.state[Scope]() = Scope("project", proj.metaDir, buf.state.get[Scope])
    }
  }

  private val psvc = msvc.service[ProjectService]
  private def root = wspace.root
  private val psdir = Files.createDirectories(root.resolve("Projects"))
  private val dsdir = Files.createDirectories(root.resolve("Depends"))

  // currently resolved projects
  private val projects = new HashMap[Root,Project]()
  // metadata on all projects added to this workspace; lazily resolved
  lazy private val (byId, toName) = {
    val byId = new HashMap[Id,Root]() ; val toName = new HashMap[Root,String]()
    Files.list(psdir).collect(Collectors.toList[Path]).foreach { dir =>
      if (Files.isDirectory(dir)) {
        try {
          val info = List() ++ Files.readAllLines(dir.resolve("info.txt"))
          val root = rootFromString(info.head)
          toName.put(root, dir.getFileName.toString)
          info.tail.flatMap(inflateId) foreach { id => byId.put(id, root) }
        } catch {
          case e :Throwable => log.log(s"Failed to resolve info for $dir: $e")
        }
      }
    }
    (byId, toName)
  }

  /** Returns the name of the workspace. */
  def name :String = wspace.name

  /** Easy access to our logger. */
  def log = msvc.log

  /** Returns `(root, name)` for all projects in this workspace. */
  def allProjects :Seq[(Root,String)] = toName.toMapV.toSeq

  /** Resolves (if necessary) and returns the project which is rooted at `root`. */
  def projectIn (root :Root) :Project = projectInRoot(root) getOrElse {
    throw Errors.feedback(s"No project in $root")
  }

  /** Resolves the project for `id`. */
  def projectFor (id :Id) :Option[Project] =
    Option(byId.get(id)).flatMap(projectInRoot) orElse psvc.resolveById(id).map(projectFromSeed)

  /** Hatches the project defined by `seed`. */
  def projectFromSeed (seed :Project.Seed) = Option(projects.get(seed.root)) || {
    val proj = msvc.injectInstance(seed.clazz, this :: seed.args)
    projects.put(proj.root, proj)
    // TODO: check project ids against byId and if any have changed, remap and updateInfo
    proj
  }

  /** Returns all currently resolved projects. */
  def loadedProjects :Seq[Project] = projects.values.toSeq

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = toName.get(proj.root) match {
    // if it's an incidental project (e.g. a random Maven dependency), use an id-based dir
    case null => dsdir.resolve(proj.idName)
    // if this is a named project in this workspace, use a dir based on its name
    case name => psdir.resolve(name)
  }

  /** The history ring for execution invocations. */
  val execHistory = new Ring(32)

  /** Handles the indexing of project source code. */
  lazy val indexer = new Indexer(this)

  /** Emits a description of this project space to `bb`. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(s"'$name' Workspace")

    bb.addSubHeader(s"All Projects")
    val allps = allProjects
    if (allps.isEmpty) bb.add("<none>")
    else bb.addKeysValues(allps.map(p => (s"${p._2} ", p._1.toString)).sorted)

    bb.addSubHeader("Loaded Projects")
    for (p <- loadedProjects) {
      bb.addSection(p.name)
      bb.addKeysValues("Kind: " -> p.getClass.getName,
                       "Root: " -> p.root.toString(),
                       "Ids: "  -> p.ids.mkString(" "),
                       "Deps: " -> p.depends.size.toString)
    }

    execs.describeSelf(bb)
  }

  /** Adds `proj` to this workspace. */
  def addProject (proj :Project) {
    if (toName.containsKey(proj.root)) throw Errors.feedback(
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
    // add this project's root to our workspace's hint path
    wspace.addHintPath(proj.root.path)
    // write this project's id info its metadata dir
    updateInfo(proj)
  }

  /** Removes `proj` from this workspace. */
  def removeProject (proj: Project) {
    // grab the project's metadata directory then remove it from toName
    val pdir = metaDir(proj)
    if (toName.remove(proj.root) == null) throw Errors.feedback(
      s"${proj.name} not added to this workspace.")
    // remove this project from our ids map
    proj.ids.foreach(byId.remove)
    // move this project's metadir out of Projects back into Depends
    Files.move(pdir, dsdir.resolve(proj.idName))
    // remove this project's root from our workspace's hint path
    wspace.removeHintPath(proj.root.path)
    // remove the project's info.txt file
    Files.deleteIfExists(pdir.resolve("info.txt"))
  }

  /** Manages the collection of Codexen for the projects in this space. */
  val codex :PSpaceCodex = new PSpaceCodex(this)

  /** Manages executions for this project space. */
  lazy val execs :Executions = new Executions(this)

  override def close () {
    projects.values.foreach(_.dispose())
    codex.close()
    wspace.state[ProjectSpace].clear()
  }

  private def projectInRoot (root :Root) =
    // the root passed here may have disappeared in the fullness of time, so validate it
    if (root == null || !Files.exists(root.path)) None
    else Option(projects.get(root)) orElse Some(resolveByPaths(List(root.path)))

  private def resolveByPaths (paths :List[Path]) :Project =
    projectFromSeed(psvc.resolveByPaths(paths))

  private def updateInfo (proj :Project) {
    val ids = metaDir(proj).resolve("info.txt")
    Files.write(ids, List(rootToString(proj.root)) ++ proj.ids.map(_.deflate))
  }
}

/** Static helpers. */
object ProjectSpace {

  /** Returns the `ProjectSpace` associated with `wspace`. */
  def apply (wspace :Workspace) :ProjectSpace = wspace.state[ProjectSpace].getOrElse {
    throw new IllegalStateException(s"No ProjectSpace configured in workspace: '$wspace'")
  }
}
