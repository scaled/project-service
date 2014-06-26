//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model.Kind
import java.nio.file.{Files, Path}
import java.util.IdentityHashMap
import reactual.{Future, Value}
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Close}

/** [[Project]]-related helper types &c. */
object Project {

  /** Represents different kinds of universal project identifiers. */
  sealed abstract class Id {
    /** Converts this Id to a string which can later be [[inflated]]. */
    def deflate :String
    protected def deflate (k :String, data :String*) = {
      val sb = new StringBuilder(k)
      for (d <- data) sb.append(Sep).append(d)
      sb.toString
    }
  }

  /** Inflates an [[Id]] from `string`. `string` must be the result of a call to [[Id.deflate]]. */
  def inflateId (string :String) = string split(Sep) match {
    case Array("r", repo, groupId, artifId, vers) => Some(RepoId(repo, groupId, artifId, vers))
    case Array("s", vcs, url) => Some(SrcURL(vcs, url))
    case Array("p", platform, vers) => Some(PlatformId(platform, vers))
    case _ => None
  }

  /** An id string to use for [[RepoId.repo]] for the Maven repository. */
  final val MavenRepo = "mvn"
  /** An id string to use for [[RepoId.repo]] for the Ivy repository. */
  final val IvyRepo = "ivy"

  /** Identifies a project via an artifact repository identifier.
    * The canonical example of this kind of ID is a Maven/Ivy style dependency. */
  case class RepoId (repo :String, groupId :String, artifactId :String,
                     version :String) extends Id {
    def deflate = deflate("r", repo, groupId, artifactId, version)
  }

  /** Identifies a project via its version control system URL. Examples:
    * - `SrcURL(git, https://github.com/scaled/maven-project.git)`
    * - `SrcURL(git, git@github.com:samskivert/samskivert.git)`
    * - `SrcURL(hg, https://ooo-maven.googlecode.com/hg/)`
    * - `SrcURL(svn, https://ooo-gwt-utils.googlecode.com/svn)`
    */
  case class SrcURL (vcs :String, url :String) extends Id {
    def deflate = deflate("s", vcs, url)
  }

  /** An id string to use for [[PlatformId.platform]] for the Java/JDK platform. Versions for this
    * platform should be of the form: `"6"`, `"7"`, `"8"`. */
  final val JavaPlatform = "jdk"

  /** Identifies a platform project. This is generally an implicit dependency added by the build
    * system, like a particular version of the JDK for a Java or Scala project, or a particular
    * version of the Ruby standard libraries for a Ruby project. */
  case class PlatformId (platform :String, version :String) extends Id {
    def deflate = deflate("p", platform, version)
  }

  // separate Id components by VT; they will be embedded in strings that are themselves separated by
  // HT, so we want to play nicely with that
  private final val Sep = 11.toChar
}

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project (val metaSvc :MetaService) {
  import Project._

  // keep a logger around for ourselves and children
  protected val log = metaSvc.log

  /** Returns the root of this project. */
  val root :Path

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** Returns the name of this project. */
  def name :String

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :Seq[Id] = Seq()

  /** Returns the ids of project's dependencies. These should be returned in highest to lowest
    * precedence order. Do not look up project dependencies manually, instead use [[depend]] which
    * will properly reference the dependent project and release it when this project hibernates. */
  def depends :Seq[Id] = Seq()

  /** Notes that `ref` is now using this project. */
  def reference (ref :Any) :this.type = {
    _refs.put(ref, ref)
    log.log(s"$this reffed by $ref") // TEMP: debug
    this
  }

  /** Returns the number of active references to this project. */
  def references :Int = _refs.size

  /** Notes that `ref` is no longer using this project. */
  def release (ref :Any) {
    if (_refs.remove(ref) == null) log.log(s"$this released by unknown referent: $ref")
    else log.log(s"$this released by $ref") // TEMP: debug
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
    if (!Files.exists(metaDir)) Files.createDirectories(metaDir)
    metaDir.resolve(name)
  }

  /** Visits a buffer containing a description of this project. */
  def visitDescription (editor :Editor, width :Int) {
    val bb = new BufferBuilder(width-1)
    describeSelf(bb)
    bb.addBlank()
    val bname = s"*project:${name}*"
    editor.visitBuffer(bb.applyTo(editor.createBuffer(bname, true, ModeInfo("help", Nil))))
  }

  /** Emits a description of this project to `bb`. The default project adds basic metadata, and
    * derived project implementations undoubtedly have useful things to add. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(name)
    bb.addBlank()

    val info = Seq.newBuilder[(String,String)]
    info += ("Root: " -> root.toString)
    ids.foreach { id => info += ("ID: " -> id.toString) }
    bb.addKeysValues(info.result :_*)

    bb.addSubHeader("Depends:")
    depends foreach { d => bb.add(d.toString) } // TODO
    if (depends.isEmpty) bb.add("<none>")

    // add info on our helpers
    compiler.describeSelf(bb)
    runner.describeSelf(bb)
  }

  /** Instructs the project to update its status info. This is generally called by project helpers
    * that participate in the modeline info. */
  def updateStatus () :Unit = status() = makeStatus

  /** Resolves (if needed), and returns the projects that correspond to `depend`. */
  def depend (depend :Id) :Option[Project] = _depprojs.get.resolve(depend)

  /** Returns the compiler that handles compilation for this project. Created on demand. */
  def compiler :Compiler = _compiler.get

  /** Returns the tester that handles test running for this project. Created on demand. */
  def tester :Tester = _tester.get

  /** Returns the runner that manages executions for this project. Created on demand. */
  def runner :Runner = _runner.get

  /** Returns the Codex for this project. Created on demand. */
  def codex :ProjectCodex = _codex.get

  override def toString = s"$name ($root)"

  /** Returns the directory in which this project will store metadata.
    * The default is to store it in `root/.scaled`. */
  protected def metaDir = root.resolve(".scaled")

  /** Returns a metadata directory in the `Projects` subdirectory of Scaled's main metadata
    * location. This is useful for projects which cannot write to their project root, though said
    * projects need to be careful not to step on one another's toes.
    */
  protected def globalMetaDir (id :String) :Path = metaSvc.metaFile("Projects").resolve(id)

  /** Shuts down all helper services and frees as much memory as possible.
    * A project hibernates when it is no longer referenced by project-mode using buffers. */
  protected def hibernate () {
    println(s"$this hibernating")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this hibernate failure", e)
    }
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

  private val _refs = new IdentityHashMap[Any,Any]() // see reference()/release()

  private val _depprojs = new Close.Box[DependMap](toClose) {
    override protected def create = new DependMap() // ctor resolves projects
  }
  class DependMap extends AutoCloseable {
    private val psvc = metaSvc.service[ProjectService]
    private val deps = MMap[Id,Project]()

    def resolve (depend :Id) :Option[Project] = deps.get(depend) match {
      case None => psvc.projectFor(depend) match {
        case sp @ Some(p) => deps.put(depend, p) ; p.reference(Project.this) ; sp
        case None => None
      }
      case sp @ Some(p) => sp
    }

    override def close () :Unit = deps.values foreach { _.release(Project.this) }
  }

  protected def createCompiler () :Compiler = new NoopCompiler()
  private val _compiler = new Close.Box[Compiler](toClose) {
    override protected def create = createCompiler()
  }
  class NoopCompiler extends Compiler(this) {
    override def describeSelf (bb :BufferBuilder) {} // nada
    override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def recompile (editor :Editor, interactive :Boolean) {
      if (interactive) editor.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer) = Future.success(true)
    override protected def nextError (buffer :Buffer, start :Loc) = None
  }

  protected def createTester () :Tester = new NoopTester()
  private val _tester = new Close.Box[Tester](toClose) {
    override protected def create = createTester()
  }
  class NoopTester extends Tester(this) {
    // override def describeSelf (bb :BufferBuilder) {} // nada
    // override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def runAllTests (editor :Editor, iact :Boolean) = false
    override def runTests (editor :Editor, iact :Boolean,
                           file :Path, typess :Seq[Model.Element]) = false
    override def runTest (editor :Editor, file :Path, elem :Model.Element) =
      editor.emitStatus("${project.name} does not provide a tester.")
  }

  protected def createRunner () :Runner = new Runner(this)
  private val _runner = new Close.Box[Runner](toClose) {
    override protected def create = createRunner()
  }

  protected def createProjectCodex () :ProjectCodex = new ProjectCodex(this)
  private val _codex = new Close.Box[ProjectCodex](toClose) {
    override protected def create = createProjectCodex()
  }
}
