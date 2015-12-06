//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import com.google.common.collect.ArrayListMultimap
import java.nio.file.{Files, Path}
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.{BufferBuilder, Errors, Properties, SubProcess}

/** Contains metadata for an execution. The metadata has is a set of key/value pairs where the
  * value can either be a single string or a sequence of strings.
  *
  * @param name the name of the execution in question.
  */
class Execution (val name :String, data :ArrayListMultimap[String,String]) {

  /** Returns the value for `key`, throwing a feedback exception if none exists. */
  def param (key :String) :String = {
    val vs = data.get(key)
    vs.size match {
      case 0 => throw Errors.feedback(s"Execution ($name) missing required parameter: '$key'")
      case 1 => vs.get(0)
      case _ => throw Errors.feedback(s"Expected single value for '$key' but got $vs")
    }
  }

  /** Returns the value for `key`, returning `defval` if none exists. */
  def param (key :String, defval :String) :String = {
    val vs = data.get(key)
    vs.size match {
      case 0 => defval
      case 1 => vs.get(0)
      case _ => throw Errors.feedback(s"Expected single value for '$key' but got $vs")
    }
  }

  /** Returns the values for `key`, returning `defvals` if none exist. */
  def param (key :String, defvals :Seq[String]) :SeqV[String] = {
    val vs = data.get(key)
    if (vs.isEmpty) defvals else vs.toSeqV
  }

  /** Used to describe this execution in the `describe-project` buffer. */
  def describe :(String, String) = (s"$name: ", data.toString)

  override def toString = s"$name $data"
}

/** Manages a set of executions for a workspace. An execution is a collection of key/value pairs
  * which configure a particular execution (usually of one or more projects' code).
  *
  * [[Execution]] keys may be unique, or they may be repeated. In the latter case they will be
  * collected into a list associated with their key. The order in which the repeats appear in the
  * file will dictate the order they appear in the list. Each execution key is prefixed by the name
  * of the execution and a dot. For example:
  *
  * ```
  * hello.runner = exec
  * hello.cmd = echo
  * hello.arg = Hello
  * hello.arg = world!
  *
  * goodbye.runner = exec
  * goodbye.cmd = echo
  * goodbye.arg = Goodbye
  * goodbye.arg = cruel
  * goodbye.arg = world!
  * ```
  *
  * Each execution is associated with a particular [[RunnerPlugin]]. The runner interprets the
  * execution configuration and performs the execution. The built-in runner, `exec`, invokes a shell
  * command in a separate process and pipes its output to a buffer. Other runners may be provided
  * via plugins with tag `runner`.
  */
class Executions (pspace :ProjectSpace) {

  private val _config = pspace.wspace.root.resolve("executions.properties")
  private val _execs = SeqBuffer[Execution]()
  private val _runners = pspace.msvc.service[PluginService].resolvePlugins[RunnerPlugin](
    "runner", List(pspace))

  private def readConfig (file :Path) :Unit = if (Files.exists(file)) {
    val configs = MMap[String,ArrayListMultimap[String,String]]()
    Properties.read(pspace.log, file) { (key, value) => key split("\\.", 2) match {
      case Array(name, ekey) => configs.getOrElseUpdate(
        name, ArrayListMultimap.create[String,String]()).put(ekey, value)
      case _ => pspace.log.log(s"$file contains invalid execution key '$key' (value = $value)")
    }}
    _execs.clear()
    configs foreach { case (name, data) => _execs += new Execution(name, data) }
  }
  // read our config and set up a file watch to re-read when it's modified
  readConfig(_config)
  pspace.wspace.toClose += pspace.msvc.service[WatchService].watchFile(_config, readConfig(_))

  /** Adds compiler info to the project info buffer. */
  def describeSelf (bb :BufferBuilder) {
    if (!executions.isEmpty) {
      bb.addSubHeader("Executions")
      bb.addKeysValues(executions.map(_.describe))
    }
  }

  /** Returns all configured executions. */
  def executions :SeqV[Execution] = _execs

  /** Invokes `exec` as a part of `project`. Output will be shown in a window matching `defgeom`
    * (which will be created if necessary). */
  def execute (exec :Execution, project :Project, defgeom :Geometry) {
    val id = exec.param("runner")
    _runners.plugins.find(_.id == id) match {
      case Some(r) => r.execute(exec, project, defgeom)
      case None => throw Errors.feedback(
        s"Unable to find runner with id '$id' for execution '${exec.name}'.")
    }
  }

  /** Generates the preamble placed at the top of a project's execution configuration file. */
  def configPreamble :Seq[String] = Seq(
    s"# '${pspace.name}' workspace executions",
    "#",
    "# Each block of 'foo.key: value' settings describes a single execution.",
    "#",
    "# NOTE: if you uncomment any of the examples, you must strip off the trailing comments",
    "# as comments are only allowed to start in column zero."
  )

  /** Opens this project's executions config file in `editor`. */
  def visitConfig (window :Window) {
    val buffer = window.workspace.openBuffer(Store(_config))
    // if the buffer is empty; populate it with an example configuration
    if (buffer.start == buffer.end) {
      println(_runners.plugins)
      val examples = _runners.plugins.flatMap(_.exampleExecutions :+ "")
      buffer.append(configPreamble ++ Seq("") ++ examples map Line.apply)
    }
    window.focus.visit(buffer)
  }
}
