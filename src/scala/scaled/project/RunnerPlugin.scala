//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.util.SubProcess

/** "Runs" an execution. */
abstract class RunnerPlugin (pspace :ProjectSpace) extends AbstractPlugin {

  /** The string which identifies this runner. Used in exec config as: `name.runner: id`. */
  def id :String

  /** Invokes `exec`, sending output to an appropriately named buffer in `window`.
    * @param project the active project when the execution was initiated.
    */
  def execute (window :Window, exec :Execution, project :Project) {
    val bufname = s"*exec:${exec.name}*"
    val buffer = window.workspace.createBuffer(Store.scratch(bufname, project.root.path),
                                               project.bufferState("log"), true)
    val cfg = config(exec)
    val info = Seq() ++ cfg.env.map { (k, v) => s"Env: $k = $v" } ++ Seq(
      s"Cwd: ${cfg.cwd}", s"Cmd: ${cfg.cmd.mkString(" ")}", "Output:")
    buffer.append(info.map(Line.apply))
    buffer.split(buffer.end)
    SubProcess(cfg, pspace.msvc.exec, buffer)
    // TODO: associate the subprocess with the buffer, kill the subprocess (if it's still alive)
    // when the buffer is killed?
    (window.workspace.frameForBuffer(buffer) || window.focus).visit(buffer)
  }

  /** Returns text describing one or more example executions. These should be in comments prefixed
    * by `#`. They will be used to pre-populated this workspace's execution file the first time a
    * user visits it. */
  def exampleExecutions :Seq[String]

  /** Creates a sub-process config for `exec`. */
  protected def config (exec :Execution) :SubProcess.Config
}

@Plugin(tag="runner")
class ExecRunnerPlugin (pspace :ProjectSpace) extends RunnerPlugin(pspace) {
  override def id = "exec"
  override def exampleExecutions = Seq(
    "# example.runner:  exec   # the runner to use for this execution",
    "# example.command: echo   # the command to be executed",
    "# example.arg:     Hello  # the first arg passed to the command",
    "# example.arg:     world. # the second arg passed to the command"
  )
  override protected def config (exec :Execution) = {
    val args = exec.param("command") +: exec.param("arg", Seq())
    SubProcess.Config(args.toArray)
  }
}
