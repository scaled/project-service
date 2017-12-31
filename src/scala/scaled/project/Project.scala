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
  def onFiles (dirs :SeqV[Path], op :Path => Unit) :Unit =
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op(file)
          FileVisitResult.CONTINUE
        }
      })
    }

  /** Used to read and write project metadata. */
  trait MetaMeta[T] {
    /** The default meta for a freshly opened and totally unknown project. */
    def zero :T
    /** Reads a `T` from [[ConfigFile]] data. */
    def read (in :Map[String,SeqV[String]]) :T
    /** Writes `meta` to `out`. */
    def write (out :ConfigFile.WriteMap, meta :T)
  }

  /** Defines the basic persistent metadata for a project. When a project is first resolved, the
    * metadata is quickly restored from a file. The project is then given a chance to refresh that
    * metadata from canonical project files (which could take a long time due to things like Gradle
    * or SBT initialization). Any time the project metadata changes, it's saved so that it can be
    * rapdily read in again next time we have a cold start. */
  case class Meta (val name :String, val ids :SeqV[Id], val sourceDirs :SeqV[Path])

  /** Handles reading and writing [[Meta]]s. */
  object Meta extends MetaMeta[Meta] {
    val zero = Meta("<unknown>", Seq(), Seq())
    def read (in :Map[String,SeqV[String]]) :Meta = {
      val Seq(name) = in("name")
      val ids = in("ids").flatMap(Codec.readId)
      val sourceDirs = in("sourceDirs").map(p => Paths.get(p))
      Meta(name, ids, sourceDirs)
    }
    def write (out :ConfigFile.WriteMap, meta :Meta) {
      out.write("name", Seq(meta.name))
      out.write("ids", meta.ids.map(Codec.showId))
      out.write("sourceDirs", meta.sourceDirs.map(_.toString))
    }
  }

  /** Identifies a component of a project, like a [[Compiler]] or a [[Tester]]. Concrete
    * [[Project]] implementations (like `MavenProject` or `GradleProject`) might add standard
    * components like `ScalaCompiler` or `JUnitTester`, which can be shared by project
    * implementations as long as they have some standard project metadata. */
  trait Component extends AutoCloseable {

    /** Adds info on this project component to the project description buffer. */
    def describeSelf (bb :BufferBuilder) :Unit
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
abstract class Project (val pspace :ProjectSpace, val root :Project.Root) {
  import Project._

  /** Tracks the basic project metadata. This should only be updated by the project, but outside
    * parties may want to react to changes to it. */
  val metaV = metaValue("meta", Meta)
  // when metaV changes, update our status
  metaV.onEmit { updateStatus() }

  /** Returns a future which is complete when this project is ready. */
  def ready :Future[Project] = _ready
  private val _ready = Promise[Project]()

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** The name of this project. */
  def name :String = metaV().name

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :SeqV[Id] = metaV().ids

  /** All top-level directories which contain source code. */
  def sourceDirs :SeqV[Path] = metaV().sourceDirs

  /** The ids of project's dependencies. These should be in highest to lowest precedence order.
    * Do not look up project dependencies manually, instead use [[depend]]. */
  def depends :SeqV[Id] = Seq()

  /** Returns a seed for this project's test companion project, if any. */
  def testSeed :Option[Seed] = None

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** A bag of closeables to be closed when this project is [[dispose]]d. */
  val toClose = Close.bag()

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Completes files in this project. */
  val fileCompleter :Completer[Store]

  /** Feedback messages (or errors) emitted on this project. These will be forwarded (by
    * project-mode) to any windows showing buffers to which this project is attached. */
  val feedback = Signal[Either[(String, Boolean), Throwable]](pspace.wspace.exec.ui)

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
    // add a lang client if one is available
    val name = buffer.store.name
    val suff = name.substring(name.lastIndexOf('.')+1).toLowerCase
    langClientFor(suff) foreach { _.onSuccess(_.addToBuffer(buffer)) }
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
  def log (msg :String) :Unit = pspace.wspace.exec.runOnUI {
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

    bb.addSubHeader("Depends:")
    val deps = depends
    deps foreach { d =>
      bb.add(Line.builder(d.toString).withLineTag(Visit.Tag(new Visit() {
        protected def go (window :Window) = pspace.projectFor(d) match {
          case None => window.popStatus(s"Unable to resolve project for $d")
          case Some(p) => p.visitDescription(window)
        }
      })).build())
    }
    if (deps.isEmpty) bb.add("<none>")

    if (!sourceDirs.isEmpty) {
      bb.addSubHeader("Build Info")
      describeBuild(bb, summarizeSources)
    }

    // add info on our helpers
    _components.values.foreach { _.describeSelf(bb) }
  }

  protected def describeMeta (bb :BufferBuilder) {
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

  /** Returns the compiler that handles compilation for this project. Created on demand. */
  def compiler :Compiler = component(classOf[Compiler]) || NoopCompiler

  /** Returns the tester that handles test running for this project. Created on demand. */
  def tester :Tester = component(classOf[Tester]) || NoopTester

  /** Resolves and returns this project's test companion project, if it has one. */
  def testCompanion :Option[Project] = testSeed.map(pspace.projectFromSeed)

  /** Returns any warnings that should be displayed when describing this project. This includes
    * things like failure to resolve project dependencies, or other configuration issues. */
  def warnings :SeqV[String] = Seq.empty

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
    println(s"$this disposing")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this dispose failure", e)
    }
    _components.values().foreach(_.close())
    _components.clear()
  }

  /** Initializes this project. Called by [[ProjectSpace]] immediately after resolution. */
  def init () {
    computeMeta(metaV()).onFailure(pspace.wspace.exec.handleError).
      onSuccess { meta => metaV() = meta ; _ready.succeed(this) }
  }

  /** Reinitializes this project. Can be called by the project when it detects that its metadata
    * has changed. */
  def reinit () {
    computeMeta(metaV()).onFailure(pspace.wspace.exec.handleError).
      onSuccess { meta => metaV() = meta }
  }

  /** Called during project resolution after metadata has been restored from the last time this
    * project was instantiated. The project should recompute its metadata (potentially on a
    * background thread) and then complete its init future with the updated metadata.
    *
    * If a project opts to `reinit` itself, this method will be called again during the reinit
    * process.
    */
  protected def computeMeta (oldMeta :Meta) :Future[Meta]

  /** Returns the component for the specified type-key, or `None` if no component is registered. */
  def component[C <: Component] (cclass :Class[C]) :Option[C] =
    Option(_components.get(cclass).asInstanceOf[C])

  /** Registers `comp` with this project. If a component of the same type-key is already registered
    * it will be closed and replaced with `comp`. Components will also be closed when this project
    * is disposed.
    */
  def addComponent[C <: Component] (cclass :Class[C], comp :C) {
    assert(comp != null)
    val oldComp = _components.get(cclass)
    if (oldComp != null) oldComp.close()
    _components.put(cclass, comp)
  }

  private val _components = new HashMap[Class[_ <: Component],Component]()

  /** Creates a meta-value with storage in this project's meta directory. */
  def metaValue[T] (id :String, metameta :MetaMeta[T]) :Value[T] = {
    val confFile = metaFile(id + ".conf")
    val value = Value(metameta.zero)
    if (Files.exists(confFile)) try {
      value() = metameta.read(ConfigFile.readMap(confFile))
    } catch {
      case t :Throwable => pspace.wspace.exec.handleError(
        new Exception(s"Failed to read project metafile '$confFile'", t))
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

  private lazy val langPlugins = metaSvc.service[PluginService].
    resolvePlugins[LangPlugin]("langserver")
  private val langClients = new HashMap[String,Future[LangClient]]()
  private def langClientFor (suff :String) :Option[Future[LangClient]] = langClients.get(suff) match {
    case null => langPlugins.plugins.filter(_.canActivate(root.path)).
      find(_.suffs(root.path).contains(suff)).map(p => {
        val client = p.createClient(this)
        p.suffs(root.path).foreach { suff => langClients.put(suff, client) }
        // TODO: close lang clients if all buffers with their suff are closed
        client onSuccess { toClose += _ }
        client onFailure pspace.wspace.exec.handleError
        client
      })
    case client => Some(client)
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

  object NoopCompiler extends Compiler(this) {
    override def describeEngine = "no-op"
    override def describeSelf (bb :BufferBuilder) {} // nada
    override def recompileOnSave = false
    override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def compile (window :Window, config :Compiler.Config) {
      if (config.interactive) window.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer, file :Option[Path]) = Future.success(true)
    override protected def nextNote (buffer :Buffer, start :Loc) = Compiler.NoMoreNotes
  }

  object NoopTester extends Tester(this) {
    // override def addStatus (sb :StringBuilder, tb :StringBuilder) {} // nada
    override def describeSelf (bb :BufferBuilder) {} // nada
    override def runAllTests (window :Window, iact :Boolean) = false
    override def runTests (window :Window, iact :Boolean,
                           file :Path, typess :SeqV[Def]) = false
    override def runTest (window :Window, file :Path, elem :Def) = {
      window.emitStatus("${project.name} does not provide a tester.")
      Future.success(())
    }
  }
}
