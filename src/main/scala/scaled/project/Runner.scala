//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.collect.ArrayListMultimap
import java.nio.file.{Files, Path}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Errors, Properties, SubProcess}

/** Contains metadata for an execution. The metadata has is a set of key/value pairs where the
  * value can either be a single string or a sequence of strings.
  * @param name the name of the execution in question.
  */
class Execution (val name :String, data :ArrayListMultimap[String,String]) {
  import scala.collection.convert.WrapAsScala._

  /** Returns the value for `key`, throwing a feedback exception if none exists. */
  def param (key :String) :String = data.get(key) match {
    case null   => throw Errors.feedback(s"Execution ($name) missing required parameter: '$key'")
    case values => if (values.size == 1) values.get(0)
                   else throw Errors.feedback(s"Expected single value for '$key' but got $values")
  }

  /** Returns the value for `key`, returning `defval` if none exists. */
  def param (key :String, defval :String) :String = data.get(key) match {
    case null   => defval
    case values => if (values.size == 1) values.get(0)
                   else throw Errors.feedback(s"Expected single value for '$key' but got $values")
  }

  /** Returns the values for `key`, returning `defvals` if none exist. */
  def param (key :String, defvals :Seq[String]) :Seq[String] = data.get(key) match {
    case null   => defvals
    case values => values
  }

  /** Used to describe this execution in the `describe-project` buffer. */
  def describe :(String, String) = (s"$name: ", data.toString)

  override def toString = s"$name $data"
}

/** Manages a set of executions for a project. An execution is a collection of key/value pairs which
  * configure a particular execution of a project's code. The `Runner` manages this metadata and
  * provides the means by which the user initiates executions.
  *
  * Execution keys may be unique, or they may be repeated. In the latter case they will be collected
  * into a list associated with their key. The order in which the repeats appear in the file will
  * dictate the order they appear in the list. Each execution key is prefixed by the name of the
  * execution and a dot. For example:
  *
  * ```
  * hello.cmd = echo
  * hello.arg = Hello
  * hello.arg = world!
  *
  * goodbye.cmd = echo
  * goodbye.arg = Goodbye
  * goodbye.arg = cruel
  * goodbye.arg = world!
  * ```
  *
  * The default execution is simply an executable and a series of command line arguments. It is
  * expected that a project will customize its `Runner` to support execution more appropriate to the
  * project. A JVM-based project, for example, would likely take a class name, JVM arguments,
  * program arguments, and run the class in a JVM configured automatically with the appropriate
  * classpath.
  */
class Runner (project :Project) extends AutoCloseable {

  private val _configFile = project.metaFile("executions.properties")
  private val _execs = ArrayBuffer[Execution]()

  private def readConfig (file :Path) :Unit = if (Files.exists(file)) {
    val configs = MMap[String,ArrayListMultimap[String,String]]()
    Properties.read(log, file) { (key, value) =>
      key split("\\.", 2) match {
        case Array(name, ekey) =>
          configs.getOrElseUpdate(name, ArrayListMultimap.create[String,String]()).put(ekey, value)
        case _ => log.log(s"$file contains invalid execution key '$key' (value = $value)")
      }
    }
    _execs.clear()
    configs foreach { case (name, data) => _execs += new Execution(name, data) }
  }
  // read our config and set up a file watch to re-read when it's modified
  readConfig(_configFile)
  project.toClose += project.metaSvc.service[WatchService].watchFile(_configFile, readConfig(_))

  /** For great logging. */
  protected val log = project.metaSvc.log

  /** Adds compiler info to the project info buffer. */
  def describeSelf (bb :BufferBuilder) {
    if (!executions.isEmpty) {
      bb.addSubHeader("Executions")
      bb.addKeysValues(executions.map(_.describe) :_*)
    }
  }

  /** Frees any resources maintained by this instance. */
  def close () {} // nada by default

  /** Returns all configured executions. */
  def executions :Seq[Execution] = _execs

  /** Invokes `exec`, sending output to an appropriately named buffer in `editor`. */
  def execute (editor :Editor, exec :Execution) {
    val bufname = s"*exec:${project.name}-${exec.name}*"
    val buffer = editor.bufferConfig(bufname).reuse().mode("log").tags("project").
      state(project.asState).create().buffer
    SubProcess(config(exec), editor, project.metaSvc.exec, buffer)
    // TODO: associate the subprocess with the buffer, kill the subprocess (if it's still alive)
    // when the buffer is killed?
    editor.visitBuffer(buffer)
  }

  /** Creates a sub-process config for `exec`. */
  protected def config (exec :Execution) :SubProcess.Config = {
    val args = exec.param("cmd") +: exec.param("arg", Seq())
    SubProcess.Config(args.toArray)
  }

  /** Generates the preamble placed at the top of a project's execution configuration file. */
  def configPreamble :Seq[String] = Seq(
    s"# ${project.name} execution config",
    "#",
    "# Each block of 'foo.key: value' settings describes a single execution."
  )

  /** Returns text describing one or more example executions. These should be in comments (prefixed
    * by #) and be pre-populated into this project's execution file the first time a user visits it.
    */
  def exampleExecutions :Seq[String] = Seq(
    "# hello.cmd: echo   # the command to be executed",
    "# hello.arg: Hello  # the first arg passed to the command",
    "# hello.arg: world. # the second arg passed to the command"
  )

  /** Opens this project's executions config file in `editor`. */
  def visitConfig (editor :Editor) {
    val buffer = editor.visitFile(Store(_configFile)).buffer
    // if the buffer is empty; populate it with an example configuration
    if (buffer.start == buffer.end) buffer.append(
      configPreamble ++ Seq("") ++ exampleExecutions ++ Seq("") map(Line.apply))
  }
}
