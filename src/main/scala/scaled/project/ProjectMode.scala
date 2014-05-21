//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import javafx.scene.control.Tooltip
import reactual.{Value, OptValue}
import scaled._
import scaled.major.EditingMode
import scaled.util.BufferBuilder

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {
  import EditorConfig._

  @Var("If true, the project will be automatically recompiled on file save.")
  val recompileOnSave = key(true)

  /** Tracks the last project/execution pair. Used by `project-reexecute. */
  val lastExecution = fnKey(cfg => OptValue[(Project,Execution)]())

  /** The history ring for selecting projects. */
  val projectHistory = fnKey(cfg => new Ring(cfg(historySize)))
}

/** A minor mode which provides fns for interacting with project files and services.
  *
  * Some stock key bindings are also redirected toward project-centric versions, for example
  * `C-x C-f` is rerouted to `find-file-in-project`. Where possible, the original fns are exposed
  * via slightly varied key bindings.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="project",
       tags=Array("project"),
       desc="""A minor mode that provides project-centric fns.""")
class ProjectMode (env :Env, psvc :ProjectService, major :EditingMode) extends MinorMode(env) {
  import ProjectConfig._

  // TODO: it's possible that our buffer's file could change and become part of a new project;
  // do we really want to handle that crazy case?
  val project :Project = psvc.projectFor(buffer.store).reference(buffer)

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => new Tooltip(s._2))))

  override def configDefs = ProjectConfig :: super.configDefs
  override def keymap = Seq(
    "C-h p" -> "describe-project",

    // file fns
    "C-x C-f" -> "project-find-file",
    "C-x C-o" -> "project-find-file-other",

    // compilation fns
    "C-c C-r" -> "project-recompile",
    "C-]"     -> "visit-next-error",
    "C-["     -> "visit-prev-error",

    // execution fns
    "C-c C-e"   -> "project-execute",
    "C-c C-a"   -> "project-execute-again",
    "C-c S-C-e" -> "project-execute-in",

    // TODO: this doens't work, we need to wire up major:find-file to route to major mode fn
    // "S-C-x S-C-f" -> "find-file"
    "S-C-x S-C-f" -> "find-file-default"
  )

  override def dispose () {
    super.dispose()
    project.release(buffer)
  }

  /** Finds a file in `proj` and visits it. */
  def findFileIn (proj :Project) {
    editor.miniRead(
      s"Find file in project (${proj.name}):", "", proj.fileHistory, proj.fileCompleter
    ) onSuccess editor.visitFile
  }

  //
  // Behaviors

  // trigger a recompile on buffer save, if thusly configured
  note(buffer.storeV onEmit {
    if (config(recompileOnSave)) project.compiler.recompile(editor, false)
  })

  //
  // FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def projectFindFile () :Unit = findFileIn(project)

  @Fn("""Reads a project name from the minibuffer, then reads a file from that project (with smart
         completion), and visits it.""")
  def projectFindFileOther () {
    val pcomp = Completer.from(psvc.knownProjects)(_._2)
    editor.miniRead(s"Project:", "", config(projectHistory), pcomp) onSuccess { case pt =>
      findFileIn(psvc.projectIn(pt._1))
    }
  }

  @Fn("TEMP: forwards find-file to major mode")
  def findFileDefault () :Unit = major.findFile()

  @Fn("""Initiates a compilation of the current project. Output from the compilation will be
         displayed in a buffer named *{project} compile* and errors identified in said output
         can be navigated using `project-next-error` and `project-previous-error`.""")
  def projectRecompile () {
    project.compiler.recompile(editor, true)
  }

  @Fn("""Visits the next compilation error. The buffer containing the compilation unit will be
         visited and the point moved to the location of the error.""")
  def visitNextError () {
    project.compiler.visitNextError(editor)
  }

  @Fn("""Visits the previous compilation error. The buffer containing the compilation unit will be
         visited and the point moved to the location of the error.""")
  def visitPrevError () {
    project.compiler.visitPrevError(editor)
  }

  @Fn("Displays the buffer that contains compiler output for this project.")
  def showCompilerOutput () {
    editor.visitBuffer(project.compiler.buffer(editor))
  }

  @Fn("Invokes a particular execution in this project.")
  def projectExecute () {
    val exns = project.runner.executions
    if (exns.isEmpty) editor.popStatus(s"${project.name} defines no executions.")
    else editor.miniRead(s"Execute:", "", project.execHistory,
                         Completer.from(exns)(_.name)) onSuccess { e => execute(project, e) }
  }

  @Fn("""Reinvokes the last invoked execution. Note that the last execution may have been in a
         different project than is currently active.""")
  def projectExecuteAgain () {
    config(lastExecution).getOption match {
      case Some((p, e)) => execute(p, e)
      case None => editor.popStatus("No execution has been invoked yet.")
    }
  }

  @Fn("Invokes a particular execution in this project.")
  def projectExecuteIn () {
    editor.popStatus("TODO")
  }

  @Fn("Visits the project's execution configuration file.")
  def projectEditExecutions () {
    project.runner.visitConfig(editor)
  }

  @Fn("Describes the current project.")
  def describeProject () {
    val bb = new BufferBuilder(view.width()-1)
    project.describeSelf(bb)
    bb.addBlank()
    val bname = s"*project:${project.name}*"
    editor.visitBuffer(bb.applyTo(editor.createBuffer(bname, true, ModeInfo("help", Nil))))
  }

  //
  // Implementation details

  private def execute (project :Project, exec :Execution) {
    project.runner.execute(editor, exec)
    config(lastExecution).update(project -> exec)
  }
}
