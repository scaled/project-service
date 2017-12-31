//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.util.SubProcess

/** "Runs" an execution. */
abstract class RunnerPlugin (pspace :ProjectSpace) extends AbstractPlugin {

  /** The string which identifies this runner. Used in exec config as: `name.runner: id`. */
  def id :String

  /** Invokes `exec`.
    * @param project the active project when the execution was initiated.
    * @param defGeom the geometry of the window in which to display output if no custom geometry is
    * specified for this execution. If the output buffer for this execution is already visible in
    * some window, that window will be used. Otherwise, if a window exists with geometry `defgeom`
    * (or the execution's custom geometry), it will be used, otherwise a new window will be created
    * with the desired geometry.
    */
  def execute (exec :Execution, project :Project, defGeom :Geometry) {
    val wspace = project.pspace.wspace
    val bufname = s"*exec:${exec.name}*"
    val buffer = wspace.createBuffer(Store.scratch(bufname, project.root.path),
                                     project.bufferState("log"), true)
    val cfg = config(exec)
    val info = Seq() ++ cfg.env.map { (k, v) => s"Env: $k = $v" } ++ Seq(
      s"Cwd: ${cfg.cwd}", s"Cmd: ${cfg.cmd.mkString(" ")}", "Output:")
    buffer.append(info.map(Line.apply))
    buffer.split(buffer.end)
    buffer.state.set(SubProcess(cfg, pspace.msvc.exec, buffer))

    val frame = (wspace.frameForBuffer(buffer) ||
                 frameForExec(wspace, Geometry(exec.param("geom", "")) || defGeom))
    frame.visit(buffer)
  }

  protected def frameForExec (wspace :Workspace, geom :Geometry) =
    (wspace.windows.find(_.geometry == geom) || wspace.openWindow(Some(geom))).focus

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
    "# example.runner:  exec      # the runner to use for this execution",
    "# example.geom:    80x40+0+0 # (optional) output window geometry",
    "# example.command: echo      # the command to be executed",
    "# example.env:     FOO=bar   # sets the 'FOO' environment variable",
    "# example.arg:     Hello     # the first arg passed to the command",
    "# example.arg:     world.    # the second arg passed to the command"
  )
  override protected def config (exec :Execution) = {
    val args = exec.param("command") +: exec.param("arg", Seq())
    SubProcess.Config(args.toArray, env=exec.paramMap("env"))
  }
}
