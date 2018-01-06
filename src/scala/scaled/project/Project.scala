//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.SourceSet
import codex.model.Def
import com.google.common.collect.{HashMultimap, Multimap}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.security.MessageDigest
import java.util.HashMap
import java.util.function.Consumer
import scala.collection.mutable.{Map => MMap}
import scala.reflect.ClassTag
import scaled._
import scaled.util.{BufferBuilder, Close, MoreFiles, Errors}

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

  /** Defines a project's root directory and any module tag that is needed to differentiate
    * multiple modules in the same root directory. Many build systems put main and test modules in
    * the same directory, and some (like SBT or Gradle) can have a whole tree of modules rooted in
    * a single directory and managed by a single build file. */
  case class Root (path :Path, module :String = "") {
    /** Returns a hash name for this root. Used for internal directory names. */
    def hashName :String = md5hex(toString)
    def toString (sep :String) = path + sep + module
    override def toString = toString(if (module.length == 0) "" else ":")
  }

  /** Returns the project configured for the supplied buffer.
    * If no project is configured in the buffer a feedback error is thrown. */
  def apply (buffer :Buffer) :Project = buffer.state.get[Project].getOrElse {
    throw Errors.feedback(s"No project configured in buffer")
  }

  /** Used to read and write project metadata. */
  trait MetaMeta[T] {
    /** The default meta for a freshly opened and totally unknown project. */
    def zero (project :Project) :T
    /** Reads a `T` from [[ConfigFile]] data. */
    def read (in :Map[String,SeqV[String]]) :T
    /** Writes `meta` to `out`. */
    def write (out :ConfigFile.WriteMap, meta :T)
  }

  /** Defines the basic persistent metadata for a project. When a project is first resolved, the
    * metadata is quickly restored from a file. Project components may subsequently update that
    * metadata based on external project files (which could take a long time due to things like
    * Gradle or SBT initialization).
    *
    * Any time the project metadata changes, it's saved so that it can be rapdily read in again
    * next time we have a cold start. */
  case class Meta (val name :String, val ids :Set[Id], val testRoot :Option[Root])

  /** Handles reading and writing [[Meta]]s. */
  object Meta extends MetaMeta[Meta] {
    def zero (project :Project) = Meta(project.root.path.getFileName.toString, Set(), None)
    def read (in :Map[String,SeqV[String]]) :Meta = {
      val Seq(name) = in("name")
      val ids = in("ids").flatMap(Codec.readId).toSet
      val testRoot = in("testRoot").map(Codec.readRoot) match {
        case Seq(root) => Some(root)
        case _         => None
      }
      Meta(name, ids, testRoot)
    }
    def write (out :ConfigFile.WriteMap, meta :Meta) {
      out.write("name", Seq(meta.name))
      out.write("ids", meta.ids.map(Codec.showId).toSeq)
      out.write("testRoot", meta.testRoot.map(Codec.showRoot).toSeq)
    }
  }

  /** Identifies a component of a project, like a [[Compiler]] or a [[Tester]]. Concrete
    * [[Project]] implementations (like `MavenProject` or `GradleProject`) might add standard
    * components like `ScalaCompiler` or `JUnitTester`, which can be shared by project
    * implementations as long as they have some standard project metadata. */
  abstract class Component extends AutoCloseable {

    /** Adds info on this project component to the project description buffer. */
    def describeSelf (bb :BufferBuilder) {}

    /** Called when the project of which this component is a part is added to `buffer`. */
    def addToBuffer (buffer :RBuffer) {}

    /** Releases any resources held by this component. */
    def close () {}
  }

  private def md5hex (text :String) = toHex(digest.digest(text.getBytes))
  private def toHex (data :Array[Byte]) = {
    val cs = new Array[Char](data.length*2)
    val chars = Chars
    var in = 0 ; var out = 0 ; while (in < data.length) {
      val b = data(in).toInt & 0xFF
      cs(out) = chars(b/16)
      cs(out+1) = chars(b%16)
      in += 1 ; out += 2
    }
    new String(cs)
  }
  private val digest = MessageDigest.getInstance("MD5")
  private final val Chars = "0123456789ABCDEF"
}

/** Provides services for a particular project.
  * @param pspace the project space of which this project is a part.
  * @param root the directory in which this project is rooted. */
class Project (val pspace :ProjectSpace, val root :Project.Root) {
  import Project._

  /** Tracks the basic project metadata. This should only be updated by the project, but outside
    * parties may want to react to changes to it. */
  val metaV = metaValue("meta", Meta)
  // when metaV changes, update our status
  metaV.onEmit { updateStatus() }

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** The name of this project. */
  def name :String = metaV().name

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :Set[Id] = metaV().ids

  /** Returns the id of this project's test companion project, if any. */
  def testRoot :Option[Root] = metaV().testRoot

  /** Resolves and returns this project's test companion project, if it has one. */
  def testCompanion :Option[Project] = testRoot.map(pspace.projectFor)

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** A bag of closeables to be closed when this project is [[dispose]]d. */
  val toClose = Close.bag()

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Feedback messages (or errors) emitted on this project. These will be forwarded (by
    * project-mode) to any windows showing buffers to which this project is attached. */
  val feedback = Signal[Either[(String, Boolean), Throwable]](pspace.wspace.exec.ui)

  /** The current analysis notes, if any. */
  val notes = Value[SeqV[Analyzer.Note]](Seq())

  /** An executor that reports errors via this project's `feedback` signal. */
  val exec = pspace.wspace.exec.handleErrors(err => feedback.emit(Right(err)))

  /** The meta service, for easy access. */
  def metaSvc :MetaService = pspace.msvc

  /** Returns the file named `name` in this project's metadata directory. */
  def metaFile (name :String) :Path = {
    metaDir.resolve(name)
  }

  /** Briefly displays a status message to the user.
    * @param ephemeral if false, the status message will also be appended to the `*messages*`
    * buffer; if true, it disappears forever in a poof of quantum decoherence. */
  def emitStatus (msg :String, ephemeral :Boolean = false) = feedback.emit(Left(msg, ephemeral))

  /** Reports an unexpected error to the user.
    * The message will also be appended to the `*messages*` buffer. */
  def emitError (err :Throwable) :Unit = feedback.emit(Right(err))

  /** Adds this project to `buffer`'s state. Called by [[ProjectSpace]] whenever a buffer is
    * created (and only after this project has reported itself as ready).
    *
    * By default adds this project to the buffer, but a project may which to inspect the path being
    * edited in the buffer and add a different project (a test companion project for example)
    * instead. */
  def addToBuffer (buffer :RBuffer) {
    buffer.state[Project]() = this
    import Config.Scope
    buffer.state[Scope]() = Scope("project", metaDir, buffer.state.get[Scope])
    buffer.state[Codex]() = codex

    // tell our components that we've been added
    _components.values.foreach { _.addToBuffer(buffer) }

    // add a lang client if one is available
    val name = buffer.store.name
    val suff = name.substring(name.lastIndexOf('.')+1).toLowerCase
    langClientFor(suff).map(_.onSuccess(_.addToBuffer(buffer)))

    // note that we've been added to this buffer
    activeBuffers += buffer
    buffer.killed.onEmit { activeBuffers -= buffer }
  }

  // tracks the buffers to which this project has been added
  private val activeBuffers = SeqBuffer[RBuffer]()

  /** Creates the buffer state for a buffer with mode `mode` and mode arguments `args`, which is
    * configured to be a part of this project. */
  def bufferState (mode :String, args :Any*) :List[State.Init[_]] = List(
    State.init(Mode.Hint(mode, args :_*)),
    State.init(classOf[Project], this))

  /** Creates the buffer state for a buffer with mode `mode` and mode arguments `args`, which is
    * configured to be a part of this project. */
  def codexBufferState (mode :String, args :Any*) :List[State.Init[_]] =
    State.init(classOf[Codex], codex) :: bufferState(mode, args :_*)

  /** Creates a simple buffer configured to be part of this project. A buffer with the same name
    * will be reused. This is useful for incidental buffers related to the project like compiler
    * output, test output, etc. */
  def createBuffer (name :String, mode :String, args :Any*) :Buffer =
    pspace.wspace.createBuffer(Store.scratch(name, root.path), bufferState(mode, args :_*), true)

  /** Creates a simple buffer configured to be part of this project. A buffer with the same name
    * will be reused. This is useful for incidental buffers related to the project like compiler
    * output, test output, etc. */
  def createBuffer (name :String, initState :List[State.Init[_]]) :Buffer =
    pspace.wspace.createBuffer(Store.scratch(name, root.path), initState, true)

  /** Returns a buffer to which incidental log output relating to this project can be sent
    * (compiler output, test output, etc.). */
  def logBuffer :Buffer = createBuffer(s"*$name:log*", "log")

  /** Appends `msg` to this project's [[logBuffer]]. This method can be called from any thread, but
    * is a bit expensive. Append to [[logBuffer]] yourself if you have a lot of logging to do and
    * know you're on the UI thread. */
  def log (msg :String) :Unit = exec.runOnUI {
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
    describeMeta(bb)

    // add info on our helpers
    _components.values.foreach { _.describeSelf(bb) }
  }

  protected def describeMeta (bb :BufferBuilder) {
    val info = Seq.builder[(String,String)]
    info += ("Impl: " -> getClass.getName)
    info += ("Root: " -> root.path.toString)
    ids.foreach { id => info += ("ID: " -> id.toString) }
    testRoot.foreach { root => info += ("Tests: " -> root.toString) }
    bb.addKeysValues(info.build())
  }

  /** Instructs the project to update its status info. This is generally called by project helpers
    * that participate in the modeline info. */
  def updateStatus () :Unit = status() = makeStatus

  // getters for various well-known project components
  def files :Filer = component[Filer] || DefaultFiler
  def sources :Sources = component[Sources] || DefaultSources
  def depends :Depends = component[Depends] || DefaultDepends
  def compiler :Compiler = component[Compiler] || DefaultCompiler
  def analyzer :Analyzer = component[Analyzer] || DefaultAnalyzer
  def tester :Tester = component[Tester] || DefaultTester

  /** Closes any open resources maintained by this project and prepares it to be freed. This
    * happens when this project's owning workspace is disposed. */
  def dispose () {
    println(s"$this disposing")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this dispose failure", e)
    }
    _components.values().foreach(_.close())
    _components.clear()
  }

  /** Returns the component for the specified type-key, or `None` if no component is registered. */
  def component[C <: Component] (cclass :Class[C]) :Option[C] =
    Option(_components.get(cclass).asInstanceOf[C])

  /** A `component` variant that uses class tags to allow usage like: `component[Foo]`. */
  def component[C <: Component] (implicit tag :ClassTag[C]) :Option[C] =
    component(tag.runtimeClass.asInstanceOf[Class[C]])

  /** Returns whether a `cclass` component has been added to this project. */
  def hasComponent[C <: Component] (cclass :Class[C]) :Boolean = _components.containsKey(cclass)

  /** Registers `comp` with this project. If a component of the same type-key is already registered
    * it will be closed and replaced with `comp`. Components will also be closed when this project
    * is disposed.
    */
  def addComponent[C <: Component] (cclass :Class[C], comp :C) {
    assert(comp != null)
    val oldComp = _components.get(cclass)
    if (oldComp != null) oldComp.close()
    _components.put(cclass, comp)
    // tell this component about buffers to which we're already added
    activeBuffers foreach comp.addToBuffer
  }

  private val _components = new HashMap[Class[_ <: Component],Component]()

  /** Creates a meta-value with storage in this project's meta directory. */
  def metaValue[T] (id :String, metameta :MetaMeta[T]) :Value[T] = {
    val confFile = metaFile(id + ".conf")
    val value = Value(metameta.zero(this))
    if (Files.exists(confFile)) try {
      value() = metameta.read(ConfigFile.readMap(confFile))
    } catch {
      case t :Throwable =>
        pspace.wspace.exec.handleError(
          new Exception(s"Failed to read meta: '$confFile' (project: ${root.path})", t))
        pspace.wspace.exec.runInBG(Files.delete(confFile))
    }
    value.onValue { nvalue =>
      val out = new ConfigFile.WriteMap(confFile)
      metameta.write(out, nvalue)
      out.close()
    }
    value
  }

  override def toString = s"$name (${root.path})"

  protected def log = metaSvc.log
  protected def codex = Codex(pspace.wspace.editor)

  private lazy val langPlugins = metaSvc.service[PluginService].
    resolvePlugins[LangPlugin]("langserver")
  private def pluginForSuff (suff :String) :Option[LangPlugin] =
    langPlugins.plugins.filter(_.canActivate(root.path)).find(_.suffs(root.path).contains(suff))

  private val langClients = new HashMap[String,Future[LangClient]]()
  private def langClientFor (suff :String) :Option[Future[LangClient]] =
    Option(langClients.get(suff)) orElse pluginForSuff(suff).map(plugin => {
      val client = plugin.createClient(this)
      plugin.suffs(root.path).foreach { suff => langClients.put(suff, client) }
      client onSuccess { client =>
        // pass lang client messages along to project
        client.messages.onValue { msg => emitStatus(s"${msg.getType}: ${msg.getMessage}") }
        toClose += client
        // TODO: close lang clients if all buffers with their suff are closed
      }
      client onFailure exec.handleError
      client
    })

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

  lazy val DefaultFiler = {
    def isZip (name :String) = (name endsWith ".zip") || (name endsWith ".jar")
    if (isZip(root.path.getFileName.toString)) new ZipFiler(Seq(root.path))
    else new DirectoryFiler(this, Ignorer.stockIgnores)
  }

  lazy val DefaultSources = new Sources(Seq())

  lazy val DefaultDepends = new Depends(this) {
    override def ids = Seq()
  }

  lazy val DefaultCompiler = new Compiler(this) {
    override def describeEngine = "no-op"
    override def recompileOnSave = false
    override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def compile (window :Window, config :Compiler.Config) {
      if (config.interactive) window.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer, file :Option[Path]) = Future.success(true)
    override protected def nextNote (buffer :Buffer, start :Loc) = Compiler.NoMoreNotes
  }

  lazy val DefaultAnalyzer = new CodexAnalyzer(codex, this)

  lazy val DefaultTester = new Tester(this) {
    // override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def runAllTests (window :Window, iact :Boolean) = false
    override def runTests (window :Window, iact :Boolean,
                           file :Path, typess :SeqV[Def]) = false
    override def runTest (window :Window, file :Path, elem :Def) = {
      window.emitStatus("${project.name} does not provide a tester.")
      Future.success(this)
    }
  }
}
