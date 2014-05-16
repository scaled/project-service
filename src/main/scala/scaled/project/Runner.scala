//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.collect.ArrayListMultimap
import java.io.File
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._
import scaled.util.{Error, Properties}

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
class Runner (project :Project, log :Logger, watchSvc :WatchService) extends AutoCloseable {
  import scala.collection.convert.WrapAsScala._

  /** Contains metadata for an execution. The metadata has is a set of key/value pairs where the
    * value can either be a single string or a sequence of strings.
    * @param name the name of the execution in question.
    */
  class Execution (val name :String, data :ArrayListMultimap[String,String]) {

    /** Returns the value `key`, returning `defval` if none was specified by the execution. */
    def param (key :String, defval :String) :String = data.get(key) match {
      case null   => defval
      case values => if (values.size == 1) values.get(0)
                     else throw Error.feedback(s"Expected single value for '$key' but got $values")
    }

    /** Returns the values `key`, returning `defvals` if none were specified by the execution. */
    def param (key :String, defvals :Seq[String]) :Seq[String] = data.get(key) match {
      case null   => defvals
      case values => values
    }

    override def toString = name
  }

  private val _configFile = project.metaFile("executions.properties")
  private val _execs = ArrayBuffer[Execution]()

  private def readConfig (file :File) :Unit = if (file.exists) {
    val configs = MMap[String,ArrayListMultimap[String,String]]()
    Properties.read(log, file) { (key, value) =>
      key split(".", 2) match {
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
  project.note(watchSvc.watchFile(_configFile, readConfig(_)))

  /** Frees any resources maintained by this instance. */
  def close () {} // nada by default

  /** Returns all configured executions. */
  def executions :Seq[Execution] = _execs

  /** Invokes `exec`, sending output to `buffer`. */
  def execute (exec :Execution, buffer :Buffer) {
    log.log(s"TODO: execute $exec")
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
    val buffer = editor.visitFile(_configFile).buffer
    // if the buffer is empty; populate it with an example configuration
    if (buffer.start == buffer.end) buffer.append(
      configPreamble ++ Seq("") ++ exampleExecutions ++ Seq("") map(Line.apply))
  }
}
