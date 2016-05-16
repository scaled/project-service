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
import scaled.major.TextConfig
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

  private val codex = Codex(wspace.editor)
  private val psvc = msvc.service[ProjectService]
  private def root = wspace.root

  // metadata for all named projects
  lazy private val pdb = new ProjectDB(root, log)

  // currently resolved projects
  private val projects = new HashMap[Root,Project]()

  /** Returns the name of the workspace. */
  def name :String = wspace.name

  /** Easy access to our logger. */
  def log = msvc.log

  /** Returns `(root, name)` for all projects in this workspace. */
  def allProjects :Seq[(Root,String)] = pdb.toInfo.values.map(_.rootName).toSeq

  /** Resolves (if necessary) and returns the project which is rooted at `root`.
    * @throws FeedbackException if the project in `root` has disappeared. */
  def reqProjectIn (root :Root) :Project = projectIn(root) getOrElse {
    throw Errors.feedback(s"No project in $root")
  }

  /** Resolves (if necessary) and returns the project which is rooted at `root`.
    * Returns `None` if the project in `root` has disappeared. */
  def projectIn (root :Root) :Option[Project] =
    // the root passed here may have disappeared in the fullness of time, so validate it
    if (root == null || !Files.exists(root.path)) None
    else Option(projects.get(root)) orElse Some(resolveByPaths(List(root.path)))

  /** Resolves the project for `id` if that project is registered our project database. Unlike
    * [[projectFor]] this will not resolve unregistered projects (usually depends). */
  def knownProjectFor (id :Id) :Option[Project] =
    Option(pdb.byId.get(id)).flatMap(projectIn)

  /** Resolves the project for `id`. */
  def projectFor (id :Id) :Option[Project] =
    knownProjectFor(id) orElse psvc.resolveById(id).map(projectFromSeed)

  /** Hatches the project defined by `seed`. */
  def projectFromSeed (seed :Project.Seed) = Option(projects.get(seed.root)) || {
    val proj = msvc.injectInstance(seed.clazz, this :: seed.args)
    projects.put(proj.root, proj)
    proj.init()
    // when the project's metadata changes...
    proj.metaV.onValueNotify { _ =>
      // make sure the project is properly mapped in the project DB
      pdb.checkInfo(proj)
      // let the codex know that it might want to index the project
      codex.checkProject(proj)
    }
    proj
  }

  /** Returns all currently resolved projects. */
  def loadedProjects :Seq[Project] = projects.values.toSeq

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = pdb.metaDir(proj)

  /** The history ring for execution invocations. */
  val execHistory = new Ring(32)

  /** Emits a description of this project space to `bb`. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(s"'$name' Workspace")

    bb.addSubHeader(s"Workspace Projects")
    val allps = allProjects
    if (allps.isEmpty) bb.add("<none>")
    else {
      val width = (0 /: allps)((m, p) => math.max(m, p._2.length))
      def pad (name :String) = name + (" " * (width-name.length))
      allps.sortBy(_._2) foreach { case (root, name) =>
        val key = pad(name)
        val lb = Line.builder(s"$key ${root.toString}").
          withLineTag(Visit.Tag(new Visit() {
            protected def go (window :Window) = reqProjectIn(root).visitDescription(window)
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
                       "Deps: " -> p.depends.size.toString)
    }

    execs.describeSelf(bb)
  }

  /** Adds `proj` to this workspace. */
  def addProject (proj :Project) {
    // add the project to our database
    if (!pdb.add(proj)) throw Errors.feedback(s"${proj.name} already added to this workspace.")
    // add this project's root to our workspace's hint path
    wspace.addHintPath(proj.root.path)
  }

  /** Removes `proj` from this workspace. */
  def removeProject (proj: Project) {
    // remove this project's root from our workspace's hint path
    wspace.removeHintPath(proj.root.path)
    // remove the project from our database
    if (!pdb.remove(proj)) throw Errors.feedback(s"${proj.name} not added to this workspace.")
  }

  /** Manages executions for this project space. */
  lazy val execs :Executions = new Executions(this)

  override def close () {
    projects.values.foreach(_.dispose())
    wspace.state[ProjectSpace].clear()
  }

  private def resolveByPaths (paths :List[Path]) :Project =
    projectFromSeed(psvc.resolveByPaths(paths))
}

/** Static helpers. */
object ProjectSpace {

  /** Returns the `ProjectSpace` associated with `wspace`. */
  def apply (wspace :Workspace) :ProjectSpace = wspace.state[ProjectSpace].getOrElse {
    throw new IllegalStateException(s"No ProjectSpace configured in workspace: '$wspace'")
  }
}
