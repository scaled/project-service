//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model.{Kind, Source}
import codex.store.{EphemeralStore, ProjectStore}
import java.nio.file.{Files, Path}
import java.util.{IdentityHashMap, ArrayList}
import reactual.{Future, Value}
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Close}

/** [[Project]]-related helper types &c. */
object Project {

  /** An id string to use for [[RepoId.repo]] for the Maven repository. */
  final val MavenRepo = "mvn"
  /** An id string to use for [[RepoId.repo]] for the Ivy repository. */
  final val IvyRepo = "ivy"

  /** Identifies a project via an artifact repository identifier.
    * The canonical example of this kind of ID is a Maven/Ivy style dependency. */
  case class RepoId (repo :String, groupId :String, artifactId :String, version :String) {
    override def toString = s"$repo:$groupId:$artifactId:$version"
  }

  /** Parses `str` into a `RepoId`. `str` must be the result of [[RepoId.toString]]. */
  def repoIdFromString (str :String) :Option[RepoId] = str.split(":", 4) match {
    case Array(r, g, a, v) => Some(RepoId(r, g, a, v))
    case _ => None
  }

  /** Identifies a project via its version control system URL. Examples:
    * - `SrcURL(git, https://github.com/scaled/maven-project.git)`
    * - `SrcURL(git, git@github.com:samskivert/samskivert.git)`
    * - `SrcURL(hg, https://ooo-maven.googlecode.com/hg/)`
    * - `SrcURL(svn, https://ooo-gwt-utils.googlecode.com/svn)`
    */
  case class SrcURL (vcs :String, url :String) {
    override def toString = s"$vcs:$url"
  }

  /** Parses `str` into a `SrcURL`. `str` must be the result of [[SrcURL.toString]]. */
  def srcURLFromString (str :String) :Option[SrcURL] = str.split(":", 2) match {
    case Array(v, u) => Some(SrcURL(v, u))
    case _ => None
  }

  /** Enumerates project dependency types. */
  sealed trait Depend {}
  /** A dependency resolved by artifact repository id. */
  case class RepoDepend (id :RepoId) extends Depend
  /** A dependency resolved by source URL. */
  case class SrcDepend (url :SrcURL) extends Depend
}

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project (val metaSvc :MetaService) {
  import Project._

  // keep a logger around for ourselves and children
  protected val log = metaSvc.log

  /** Returns the name of this project. */
  def name :String

  /** Returns the root of this project. */
  def root :Path

  /** Returns the artifact repository identifier for this project, if it has one. */
  def id :Option[RepoId] = None

  /** Returns the version control source URL for this project, if it has one. */
  def sourceURL :Option[SrcURL] = None

  /** Returns this project's dependencies. These should be returned in highest to lowest precedence
    * order. Do not look up project dependencies manually, instead use [[depend]] which will
    * properly reference the dependent project and release it when this project hibernates. */
  def depends :Seq[Depend] = Seq()

  /** Notes that `ref` is now using this project. */
  def reference (ref :Any) :this.type = {
    _refs.put(ref, ref)
    this
  }

  /** Notes that `ref` is no longer using this project. */
  def release (ref :Any) {
    if (_refs.remove(ref) == null) log.log(s"$this released by unknown referent: $ref")
    if (_refs.isEmpty) hibernate()
  }

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** The history ring for execution invocations. */
  val execHistory = new Ring(32)

  /** The history rings for Codex completions. */
  val codexHistory = Map(Kind.MODULE -> new Ring(32),
                         Kind.TYPE   -> new Ring(32),
                         Kind.FUNC   -> new Ring(32),
                         Kind.VALUE  -> new Ring(32))

  /** Completes files in this project. The string representation of the files should not be
    * prefixed with path information, but rather suffixed and only where necessary to avoid
    * name collisions.
    *
    * Thus one might see as possible completions: `Bar.scala Baz.scala(util/) Baz.scala(data/)`
    * When completing on `Ba`.
    */
  val fileCompleter :Completer[Store]

  /** A bag of closeables that will be closed when this project hibernates. */
  val toClose = Close.bag()

  /** Returns the file named `name` in this project's metadata directory. */
  def metaFile (name :String) :Path = {
    if (!Files.exists(_metaDir)) Files.createDirectory(_metaDir)
    _metaDir.resolve(name)
  }

  /** Emits a description of this project to `bb`. The default project adds basic metadata, and
    * derived project implementations undoubtedly have useful things to add. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(name)
    bb.addBlank()

    val info = Seq.newBuilder[(String,String)]
    info += ("Root: " -> root.toString)
    id.foreach { id => info += ("ID: " -> id.toString) }
    sourceURL.foreach { url => info += ("Source: " -> url.toString) }
    bb.addKeysValues(info.result :_*)

    // add info on our helpers
    compiler.describeSelf(bb)
    runner.describeSelf(bb)
  }

  /** Instructs the project to update its status info. This is generally called by project helpers
    * that participate in the modeline info. */
  def updateStatus () :Unit = status() = makeStatus

  /** Resolves (if needed), and returns the projects that correspond to `depend`. */
  def depend (depend :Depend) :Option[Project] = _depprojs.get.resolve(depend)

  /** Returns the compiler that handles compilation for this project. Created on demand. */
  def compiler :Compiler = _compiler.get

  /** Returns the tester that handles test running for this project. Created on demand. */
  def tester :Tester = _tester.get

  /** Returns the runner that manages executions for this project. Created on demand. */
  def runner :Runner = _runner.get

  /** Returns the Codex for this project. Created on demand. */
  def codex :Codex = _codex.get

  /** Returns the Codex project store for this project. Created on demand. */
  def projectStore :ProjectStore = _pstore.get

  /** Requests that the specified source file be indexed. */
  def indexSource (source :Source, buffer :BufferV) :Option[SourceIndex] = None

  override def toString = s"Project($root, $name, $id, $sourceURL)"

  /** Shuts down all helper services and frees as much memory as possible.
    * A project hibernates when it is no longer referenced by project-mode using buffers. */
  protected def hibernate () {
    toClose.close()
  }

  /** Populates our status line (`sb`) and status line tooltip (`tb`) strings. */
  protected def makeStatus (sb :StringBuilder, tb :StringBuilder) {
    compiler.addStatus(sb, tb)
  }
  private def makeStatus :(String,String) = {
    val sb = new StringBuilder("(").append(name)
    val tb = new StringBuilder("Current project: ").append(name)
    makeStatus(sb, tb)
    (sb.append(")").toString, tb.toString)
  }

  protected def createCompiler () :Compiler = new Compiler(this) {
    override def describeSelf (bb :BufferBuilder) {} // nada
    override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def recompile (editor :Editor, interactive :Boolean) {
      if (interactive) editor.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer) = Future.success(true)
    override protected def nextError (buffer :Buffer, start :Loc) = None
  }

  protected def createTester () :Tester = new Tester(this) {
    // override def describeSelf (bb :BufferBuilder) {} // nada
    // override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def runAllTests (editor :Editor, iact :Boolean) = false
    override def runTests (editor :Editor, iact :Boolean,
                           file :Path, typess :Seq[Model.Element]) = false
    override def runTest (editor :Editor, file :Path, elem :Model.Element) =
      editor.emitStatus("${project.name} does not provide a tester.")
  }

  protected def createRunner () :Runner = new Runner(this)

  protected def createProjectStore () :ProjectStore = new EphemeralStore()

  /** Resolves the project stores for our Codex. */
  protected def resolveProjectStores :ArrayList[ProjectStore] = {
    val list = new ArrayList[ProjectStore]()
    list.add(projectStore)
    for (dep <- depends) depend(dep) match {
      case Some(p) => list.add(p.projectStore)
      case None => resolveNonProjectStore(dep) foreach(list.add)
    }
    list
  }

  /** Resolves the project store for a dependency for which a Scaled project was unavailable. If
    * `None` is returned, this dependency will be omitted from the Codex. */
  protected def resolveNonProjectStore (depend :Depend) :Option[ProjectStore] = None

  private val _refs = new IdentityHashMap[Any,Any]() // see reference()/release()
  private val _metaDir = root.resolve(".scaled")

  class DependMap extends AutoCloseable {
    private val psvc = metaSvc.service[ProjectService]
    private val deps = MMap[Depend,Project]()

    def resolve (depend :Depend) :Option[Project] = deps.get(depend) match {
      case None => psvc.projectFor(depend) match {
        case sp @ Some(p) => deps.put(depend, p) ; sp
        case None => None
      }
      case sp @ Some(p) => sp
    }

    override def close () :Unit = deps.values foreach { _.release(Project.this) }
  }
  private val _depprojs = new Close.Box[DependMap](toClose) {
    override protected def create = new DependMap() // ctor resolves projects
  }

  private val _compiler = new Close.Box[Compiler](toClose) {
    override protected def create = createCompiler()
  }
  private val _tester = new Close.Box[Tester](toClose) {
    override protected def create = createTester()
  }
  private val _runner = new Close.Box[Runner](toClose) {
    override protected def create = createRunner()
  }

  private val _codex = new Close.Ref[Codex](toClose) {
    override protected def create = new Codex(resolveProjectStores)
  }
  private val _pstore = new Close.Box[ProjectStore](toClose) {
    override protected def create = createProjectStore()
  }
}
