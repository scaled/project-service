//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.SourceSet
import codex.model.{Def, Kind}
import codex.store.MapDBStore
import com.google.common.collect.{HashMultimap, Multimap}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.util.HashMap
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Close}

/** [[Project]]-related helper types &c. */
object Project {

  /** Represents different kinds of universal project identifiers. */
  sealed abstract class Id

  /** An id string to use for [[RepoId.repo]] for the Maven repository. */
  final val MavenRepo = "mvn"
  /** An id string to use for [[RepoId.repo]] for the Ivy repository. */
  final val IvyRepo = "ivy"

  /** Identifies a project via an artifact repository identifier.
    * The canonical example of this kind of ID is a Maven/Ivy style dependency. */
  case class RepoId (repo :String, groupId :String, artifactId :String,
                     version :String) extends Id

  /** Identifies a project via its version control system URL. Examples:
    * - `SrcURL(git, https://github.com/scaled/maven-project.git)`
    * - `SrcURL(git, git@github.com:samskivert/samskivert.git)`
    * - `SrcURL(hg, https://ooo-maven.googlecode.com/hg/)`
    * - `SrcURL(svn, https://ooo-gwt-utils.googlecode.com/svn)`
    */
  case class SrcURL (vcs :String, url :String) extends Id

  /** An id string to use for [[PlatformId.platform]] for the Java/JDK platform. Versions for this
    * platform should be of the form: `"6"`, `"7"`, `"8"`. */
  final val JavaPlatform = "jdk"

  /** Identifies a platform project. This is generally an implicit dependency added by the build
    * system, like a particular version of the JDK for a Java or Scala project, or a particular
    * version of the Ruby standard libraries for a Ruby project. */
  case class PlatformId (platform :String, version :String) extends Id

  /** Defines a project's root directory and whether or not we're viewing the project in test mode.
    * Many build systems conflate test configuration with normal build configuration, but Scaled
    * likes to treat those as two separate projects (since nearly all the configuration is
    * duplicated for a project's test "mode"). */
  case class Root (path :Path, testMode :Boolean) {
    override def toString = if (testMode) s"$path (test)" else s"$path"
  }

  // needed because the wildcard in Seed's ctor (Class) results in existential in unapply
  import scala.language.existentials

  /** A seed which can be used to instantiate a project. These are returned by
    * [[ProjectFinderPlugin]]s when resolving a project. */
  case class Seed (root :Root, name :String, intelligent :Boolean,
                   clazz :Class[_ <: Project], args :List[Any])

  /** Returns the project configured for the supplied buffer. */
  def apply (buffer :RBuffer) :Project = buffer.state[Project].getOrElse {
    throw new IllegalStateException(s"No project configured in buffer: '$buffer'")
  }

  /** Applies `op` to all files in the directory trees rooted at `dirs`. */
  def onFiles (dirs :Seq[Path], op :Path => Unit) :Unit =
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op(file)
          FileVisitResult.CONTINUE
        }
      })
    }
}

/** Provides services for a particular project. */
abstract class Project (val pspace :ProjectSpace) {
  import Project._

  /** Returns the root of this project. */
  val root :Root

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** Returns the name of this project. */
  def name :String

  /** Returns a unique name to use for this project when it has been resolved automatically as a
    * dependency of another project and has no user-facing name. This is used to create a
    * directory, so the name must not include non-filesystem-safe characters. */
  def idName :String

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :Seq[Id] = Seq()

  /** Returns the ids of project's dependencies. These should be returned in highest to lowest
    * precedence order. Do not look up project dependencies manually, instead use [[depend]] which
    * will properly reference the dependent project and release it when this project hibernates. */
  def depends :Seq[Id] = Seq()

  /** Returns a seed for this project's test companion project, if any. */
  def testSeed :Option[Seed] = None

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** A bag of closeables to be closed when this project [[hibernate]]s or is [[dispose]]d. */
  val toClose = Close.bag()

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Completes files in this project. */
  val fileCompleter :Completer[Store]

  /** The meta service, for easy access. */
  def metaSvc :MetaService = pspace.msvc

  /** Returns the file named `name` in this project's metadata directory. */
  def metaFile (name :String) :Path = {
    metaDir.resolve(name)
  }

  /** Creates the buffer state for a buffer with mode `mode` and mode arguments `args`, which is
    * configured to be a part of this project. */
  def bufferState (mode :String, args :Any*) :List[State.Init[_]] = List(
    State.init(Mode.Hint(mode, args :_*)),
    State.init(classOf[Project], this))

  /** Creates a simple buffer configured to be part of this project. A buffer with the same name
    * will be reused. This is useful for incidental buffers related to the project like compiler
    * output, test output, etc. */
  def createBuffer (name :String, mode :String, args :Any*) :Buffer =
    pspace.wspace.createBuffer(Store.scratch(name, root.path), bufferState(mode, args :_*), true)

  /** Returns a buffer to which incidental log output relating to this project can be sent
    * (compiler output, test output, etc.). */
  def logBuffer :Buffer = createBuffer(s"*$name:log*", "log")

  /** Appends `msg` to this project's [[logBuffer]]. This method can be called from any thread, but
    * is a bit expensive. Append to [[logBuffer]] yourself if you have a lot of logging to do and
    * know you're on the UI thread. */
  def log (msg :String) :Unit = pspace.wspace.editor.exec.runOnUI(pspace.wspace) {
    logBuffer.append(Line.fromTextNL(msg))
  }

  /** Visits a buffer containing a description of this project. */
  def visitDescription (window :Window) {
    val buf = createBuffer(s"*project:${name}*", "help")
    val bb = new BufferBuilder(window.focus.geometry.width-1)
    describeSelf(bb)
    window.focus.visit(bb.applyTo(buf))
  }

  /** Emits a description of this project to `bb`. The default project adds basic metadata, and
    * derived project implementations undoubtedly have useful things to add. */
  def describeSelf (bb :BufferBuilder) {
    bb.addHeader(name)
    bb.addBlank()

    val info = Seq.builder[(String,String)]
    info += ("Impl: " -> getClass.getName)
    info += ("Root: " -> root.path.toString)
    ids.foreach { id => info += ("ID: " -> id.toString) }
    testSeed.foreach { seed => info += ("Tests: " -> seed.root.path.toString) }
    bb.addKeysValues(info.build())

    // if we have warnings, display them
    val ws = warnings
    if (!ws.isEmpty) {
      bb.addSubHeader("Warnings:")
      ws foreach { bb.add(_) }
    }

    bb.addSubHeader("Depends:")
    depends foreach { d =>
      bb.add(Line.builder(d.toString).withLineTag(Visit.Tag(new Visit() {
        protected def go (window :Window) = depend(d) match {
          case None => window.popStatus(s"Unable to resolve project for $d")
          case Some(p) => p.visitDescription(window)
        }
      })).build())
    }
    if (depends.isEmpty) bb.add("<none>")

    if (!sourceDirs.isEmpty) {
      bb.addSubHeader("Build Info")
      describeBuild(bb, summarizeSources)
    }

    // add info on our helpers
    store.describeSelf(bb)
    compiler.describeSelf(bb)
  }

  protected def describeBuild (bb :BufferBuilder, srcsum :Map[String,SourceSet]) {
    bb.addSection("Source dirs:")
    bb.addKeysValues("compile: " -> sourceDirs.mkString(" "))
    if (!srcsum.isEmpty) {
      bb.addSection("Source files:")
      bb.addKeysValues(srcsum.map((suff, srcs) => (s".$suff: ", srcs.size.toString)))
      bb.addSection("Compiler options:")
      compiler.describeOptions(bb)
    }
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

  /** Resolves and returns this project's test companion project, if it has one. */
  def testCompanion :Option[Project] = testSeed.map(pspace.projectFromSeed)

  /** Returns any warnings that should be displayed when describing this project. This includes
    * things like failure to resolve project dependencies, or other configuration issues. */
  def warnings :Seq[String] = Seq.empty

  /** A [[ProjectStore]] that maintains a reference back to its owning project. */
  class CodexStore extends MapDBStore(name, metaFile("codex")) {
    def owner :Project = Project.this
    def isEmpty :Boolean = !topLevelDefs.iterator.hasNext
    def describeSelf (bb :BufferBuilder) {
      bb.addSubHeader("Codex:")
      bb.add(Line.builder(s"Defs: $defCount").withLineTag(Visit.Tag(new Visit() {
        protected def go (window :Window) = CodexSummaryMode.visitTopLevel(window, CodexStore.this)
      })).build())
    }
    def reindex () :Unit = pspace.indexer.queueReindexAll(Project.this)
    override def toString = s"codex:$name"
    // when we're resolved, potentially trigger a full initial index
    if (isEmpty) reindex()
  }

  /** Returns all top-level directories which contain source code. */
  def sourceDirs :Seq[Path] = Seq()

  /** Returns the Codex store for this project. Created on demand. */
  def store :CodexStore = _store.get

  /** Applies `op` to all files in this project. The default implementation applies `op` to all
    * files in [[root]] directory and its subdirectories. Subclasses may refine this result. */
  def onFiles (op :Path => Unit) :Unit = Project.onFiles(Seq(root.path), op)

  /** Applies `op` to all source files in this project.
    * @param forTest if true `op` is applied to the test sources, if false the main sources. */
  def onSources (op :Path => Unit) :Unit = Project.onFiles(sourceDirs, op)

  /** Returns a map of all source files in this project, grouped by file suffix. */
  def summarizeSources :Map[String,SourceSet] = {
    val bySuff = HashMultimap.create[String,Path]()
    onSources { file =>
      val fname = file.getFileName.toString
      fname.lastIndexOf(".") match {
        case -1 => // skip it!
        case ii => bySuff.put(fname.substring(ii+1), file)
      }
    }
    val mb = Map.builder[String,SourceSet]()
    bySuff.asMap.entrySet.toSetV foreach { ent =>
      mb += (ent.getKey, SourceSet.create(ent.getValue, ent.getValue.size))
    }
    mb.build()
  }

  /** Closes any open resources maintained by this project and prepares it to be freed. This
    * happens when this project's owning workspace is disposed. */
  def dispose () {
    hibernate()
  }

  override def toString = s"$name (${root.path})"
  protected def log = metaSvc.log

  /** Causes this project to free up ephemeral resources, which will be recreated if the project is
    * once again called into service. */
  protected def hibernate () {
    println(s"$this hibernating")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this hibernate failure", e)
    }
  }

  /** Returns the directory in which this project will store metadata. */
  private[project] def metaDir = {
    val dir = pspace.metaDir(this)
    if (!Files.exists(dir)) Files.createDirectories(dir)
    dir
  }

  /** Populates our status line (`sb`) and status line tooltip (`tb`) strings. */
  protected def makeStatus (sb :StringBuilder, tb :StringBuilder) {
    compiler.addStatus(sb, tb)
    testCompanion foreach { tproj => tproj.compiler.addStatus(sb, tb) }
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
    private val deps = new HashMap[Id,Project]()

    def resolve (depend :Id) :Option[Project] = deps.get(depend) match {
      case null => pspace.projectFor(depend) match {
        case sp @ Some(p) => deps.put(depend, p) ; sp
        case None => None
      }
      case proj => Some(proj)
    }
  }

  protected def createCompiler () :Compiler = new NoopCompiler()
  private val _compiler = new Close.Box[Compiler](toClose) {
    override protected def create = createCompiler()
  }
  class NoopCompiler extends Compiler(this) {
    override def describeSelf (bb :BufferBuilder) {} // nada
    override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def compile (window :Window, config :Compiler.Config) {
      if (config.interactive) window.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer, file :Option[Path]) = Future.success(true)
    override protected def nextNote (buffer :Buffer, start :Loc) = Compiler.NoMoreNotes
  }

  protected def createTester () :Tester = new NoopTester()
  private val _tester = new Close.Box[Tester](toClose) {
    override protected def create = createTester()
  }
  class NoopTester extends Tester(this) {
    // override def describeSelf (bb :BufferBuilder) {} // nada
    // override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def runAllTests (window :Window, iact :Boolean) = false
    override def runTests (window :Window, iact :Boolean,
                           file :Path, typess :Seq[Def]) = false
    override def runTest (window :Window, file :Path, elem :Def) = {
      window.emitStatus("${project.name} does not provide a tester.")
      Future.success(())
    }
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
}
