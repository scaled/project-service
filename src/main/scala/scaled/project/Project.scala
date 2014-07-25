//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import codex.store.MapDBStore
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, SimpleFileVisitor}
import reactual.{Future, Value}
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Close, Reffed}

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

  /** A seed which can be used to instantiate a project. These are returned by
    * [[ProjectFinderPlugin]]s when resolving a project. */
  case class Seed (root :Path, intelligent :Boolean, clazz :Class[_ <: Project], args :List[Any])

  // separate Id components by VT; they will be embedded in strings that are themselves separated by
  // HT, so we want to play nicely with that
  private final val Sep = 11.toChar
}

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project (val pspace :ProjectSpace) extends Reffed {
  import Project._

  /** Returns the root of this project. */
  val root :Path

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** Returns the name of this project. */
  def name :String

  /** Returns a unique name to use for this project when it has been resolved automatically as a
    * dependency of another project and has no user-facing name. This is used to create a directory,
    * so the name must not include non-filesystem-safe characters. */
  def idName :String

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :Seq[Id] = Seq()

  /** Returns the ids of project's dependencies. These should be returned in highest to lowest
    * precedence order. Do not look up project dependencies manually, instead use [[depend]] which
    * will properly reference the dependent project and release it when this project hibernates. */
  def depends :Seq[Id] = Seq()

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** The history ring for execution invocations. */
  val execHistory = new Ring(32)

  /** Completes files in this project. The string representation of the files should not be
    * prefixed with path information, but rather suffixed and only where necessary to avoid
    * name collisions.
    *
    * Thus one might see as possible completions: `Bar.scala Baz.scala(util/) Baz.scala(data/)`
    * When completing on `Ba`.
    */
  val fileCompleter :Completer[Store]

  /** The meta service, for easy access. */
  def metaSvc :MetaService = pspace.msvc

  /** Returns the file named `name` in this project's metadata directory. */
  def metaFile (name :String) :Path = {
    if (!Files.exists(metaDir)) Files.createDirectories(metaDir)
    metaDir.resolve(name)
  }

  /** Creates a state initializer for this project. */
  def asState = State.init(classOf[Project], this)

  /** Visits a buffer containing a description of this project. */
  def visitDescription (editor :Editor) {
    val bname = s"*project:${name}*"
    val view = editor.bufferConfig(bname).
      reuse().mode("help").tags("project").state(asState).create()
    val bb = new BufferBuilder(view.width()-1)
    describeSelf(bb)
    editor.visitBuffer(bb.applyTo(view))
  }

  /** Emits a description of this project to `bb`. The default project adds basic metadata, and
    * derived project implementations undoubtedly have useful things to add. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(name)
    bb.addBlank()

    val info = Seq.newBuilder[(String,String)]
    info += ("Impl: " -> getClass.getName)
    info += ("Root: " -> root.toString)
    ids.foreach { id => info += ("ID: " -> id.toString) }
    bb.addKeysValues(info.result :_*)

    bb.addSubHeader("Depends:")
    depends foreach { d => bb.add(d.toString) } // TODO
    if (depends.isEmpty) bb.add("<none>")

    // add info on our helpers
    store.describeSelf(bb)
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

  /** A [[ProjectStore]] that maintains a reference back to its owning project. */
  class CodexStore extends MapDBStore(name, metaFile("codex")) {
    def owner :Project = Project.this
    def isEmpty :Boolean = !topLevelDefs.iterator.hasNext
    def describeSelf (bb :BufferBuilder) {
      bb.addSubHeader("Codex:")
      bb.addKeysValues("Defs: " -> defCount.toString,
                       "Names: " -> nameCount.toString)
    }
    override def toString = s"codex:$name"
    // when we're resolved, potentially trigger a full initial index
    if (isEmpty) indexer.queueReindexAll()
  }

  /** Returns the Codex store for this project. Created on demand. */
  def store :CodexStore = _store.get

  /** Returns the indexer used by this project.
    * Created lazily, but never released because indexers don't maintain runtime state. */
  lazy val indexer :Indexer = createIndexer()

  /** Applies `op` to all files in this project. The default implementation applies `op` to all
    * files in [[root]] directory and its subdirectories. Subclasses may refine this result. */
  def onFiles (op :Path => Unit) {
    Files.walkFileTree(root, new SimpleFileVisitor[Path]() {
      override def visitFile (file :Path, attrs :BasicFileAttributes) = {
        if (!Files.isDirectory(file)) op(file)
        FileVisitResult.CONTINUE
      }
    })
  }

  override def toString = s"$name ($root)"
  override protected def log = metaSvc.log

  /** Returns the directory in which this project will store metadata. */
  protected def metaDir = pspace.metaDir(this)

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

  private val _depprojs = new Close.Ref[DependMap](toClose) {
    override protected def create = new DependMap() // ctor resolves projects
  }
  class DependMap {
    private val deps = MMap[Id,Project]()

    def resolve (depend :Id) :Option[Project] = deps.get(depend) match {
      case None => pspace.projectFor(depend) match {
        case sp @ Some(p) => deps.put(depend, p) ; toClose += p.reference(Project.this) ; sp
        case None => None
      }
      case sp @ Some(p) => sp
    }
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

  protected def createProjectStore () :CodexStore = new CodexStore()
  private val _store = new Close.Box[CodexStore](toClose) {
    override protected def create = createProjectStore()
    override protected def willClose (ref :CodexStore) {
      // don't call super, which would close our project store directly, instead delegate closure
      // to a background thread; MapDB close can block for a non-trivial duration
      if (ref != null) metaSvc.exec.runInBG { ref.close() }
    }
  }

  protected def createIndexer () :Indexer = new Indexer(this)
}
