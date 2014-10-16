//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import javafx.scene.control.Tooltip
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.ReadingMode
import scaled.util.BufferBuilder

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {
  import EditorConfig._

  @Var("If true, the project will be automatically recompiled on file save.")
  val recompileOnSave = key(true)

  @Var("If true, the test output buffer will be shown when tests are run interactively.")
  val showOutputOnTest = key(false)

  /** Tracks the last execution. Used by `workspace-execute-again`. */
  val lastExecution = fnKey(cfg => OptValue[Execution]())

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
@Minor(name="project", tags=Array("project"),
       desc="""A minor mode that provides project-centric fns.""")
class ProjectMode (env :Env, major :ReadingMode) extends MinorMode(env) {
  import ProjectConfig._

  val pspace = ProjectSpace(env)
  // TODO: it's possible that our buffer's file could change and become part of a new project;
  // do we really want to handle that crazy case?
  val project = pspace.project(buffer)
  note(project.reference(this))

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => new Tooltip(s._2))))

  override def configDefs = ProjectConfig :: super.configDefs
  override def keymap = super.keymap.
    bind("C-h p", "describe-project").
    bind("C-h w", "describe-workspace").

    // file fns
    bind("C-x C-f", "project-find-file").
    bind("C-x C-o", "project-find-file-other").

    // navigation fns
    bind("C-]",     "project-next-error-or-failure").
    bind("C-[",     "project-prev-error-or-failure").

    // compilation fns
    bind("C-c C-r", "project-recompile").

    // test fns
    bind("C-c S-C-t", "project-run-all-tests").
    bind("C-c C-t",   "project-run-file-tests").

    // execution fns
    bind("C-c C-e",   "workspace-execute").
    bind("C-c C-a",   "workspace-execute-again").

    // TODO: this doesn't work, we need to wire up major:find-file to route to major mode fn
    // bind("S-C-x S-C-f", "find-file")
    bind("S-C-x S-C-f", "find-file-default");

  /** Finds a file in `proj` and visits it. */
  def findFileIn (proj :Project) {
    editor.mini.read(
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
  // General FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def projectFindFile () :Unit = findFileIn(project)

  @Fn("""Reads a project name from the minibuffer, then reads a file from that project (with smart
         completion), and visits it.""")
  def projectFindFileOther () {
    val pcomp = Completer.from(pspace.allProjects)(_._2)
    editor.mini.read(s"Project:", "", config(projectHistory), pcomp) onSuccess { case pt =>
      findFileIn(pspace.projectIn(pt._1))
    }
  }

  @Fn("TEMP: forwards find-file to major mode")
  def findFileDefault () :Unit = major.findFile()

  @Fn("""Invokes `project-next-error` if there are any compilation errors, `project-next-failure`
         if there are no compilation errors, but are test failures.""")
  def projectNextErrorOrFailure () {
    if (project.compiler.errors.count > 0) project.compiler.errors.visitNext(editor)
    else if (project.tester.failures.count > 0) project.tester.failures.visitNext(editor)
    else editor.popStatus("There are currently no known compilation errors or test failures.")
  }

  @Fn("""Invokes `project-prev-error` if there are any compilation errors, `project-prev-failure`
         if there are no compilation errors, but are test failures.""")
  def projectPrevErrorOrFailure () {
    if (project.compiler.errors.count > 0) project.compiler.errors.visitPrev(editor)
    else if (project.tester.failures.count > 0) project.tester.failures.visitPrev(editor)
    else editor.popStatus("There are currently no known compilation errors or test failures.")
  }

  //
  // Compile FNs

  @Fn("""Initiates a compilation of the current project. Output from the compilation will be
         displayed in a buffer named *compile:{project}* and errors identified in said output
         can be navigated using `project-next-error` and `project-previous-error`.""")
  def projectRecompile () {
    project.compiler.recompile(editor, true)
  }

  @Fn("""Resets the compiler for this project. This can be useful if the compiler misbehaves,
         due perhaps to a command line compiler stomping on its files or something similar.""")
  def projectResetCompiler () {
    project.compiler.reset()
  }

  @Fn("""Visits the next compilation error. The buffer containing the compilation unit will be
         visited and the point moved to the location of the error.""")
  def projectNextError () {
    project.compiler.errors.visitNext(editor)
  }

  @Fn("""Visits the previous compilation error. The buffer containing the compilation unit will be
         visited and the point moved to the location of the error.""")
  def projectPrevError () {
    project.compiler.errors.visitPrev(editor)
  }

  @Fn("Displays the buffer that contains compiler output for this project.")
  def showCompilerOutput () {
    editor.visitBuffer(project.compiler.buffer(editor))
  }

  //
  // Test FNs

  @Fn("""Runs all of this project's tests. Output is displayed in a buffer named *test{project}*.
         Failures identified in said output can be navigated using `project-next-failure` and
         `project-previous-failure`.""")
  def projectRunAllTests () {
    if (project.tester.runAllTests(editor, true)) maybeShowTestOutput()
    else editor.popStatus("No tests were found.")
  }

  @Fn("""Identifies the test file associated with the current buffer (which may be the buffer's file
         itself if that file contains tests) and runs the tests in it. If no associated test buffer
         can be located, we fall back to running all tests for the project.
         See project-run-all-tests for info on test output and failure navigation.""")
  def projectRunFileTests () {
    buffer.store.file match {
      case None => editor.popStatus(
        "This buffer has no associated file. A file is needed to detect tests.")
      case Some(file) =>
        val ran = project.tester.findTestFile(file) match {
          case None        => project.tester.runAllTests(editor, true)
          case Some(tfile) =>
            val telems = Seq[Model.Element]() // TODO: ask project for test elements in file
            project.tester.runTests(editor, true, tfile, telems)
        }
        if (ran) maybeShowTestOutput()
        else editor.emitStatus("No tests were found.")
    }
  }

  @Fn("""Forcibly aborts any tests in progress and terminates any daemon currently being used
         to run this project's tests.""")
  def projectAbortTests () {
    project.tester.abort()
  }

  @Fn("""Visits the next test failure. The buffer containing the failing test code will be
         visited and the point moved to the location indicated by the test output.""")
  def projectNextFailure () {
    project.compiler.errors.visitNext(editor)
  }

  @Fn("""Visits the previous test failure. The buffer containing the failing test code will be
         visited and the point moved to the location indicated by the test output.""")
  def projectPrevFailure () {
    project.compiler.errors.visitPrev(editor)
  }

  @Fn("Displays the buffer that contains test output for this project.")
  def showTestOutput () {
    editor.visitBuffer(project.tester.buffer(editor))
  }

  //
  // Execute FNs

  @Fn("Invokes a particular execution in this workspace.")
  def workspaceExecute () {
    val exns = pspace.execs.executions
    if (exns.isEmpty) editor.popStatus(s"${pspace.name} defines no executions.")
    else editor.mini.read(s"Execute:", "", pspace.execHistory,
                          Completer.from(exns)(_.name)) onSuccess execute
  }

  @Fn("""Reinvokes the last invoked execution.""")
  def workspaceExecuteAgain () {
    config(lastExecution).getOption match {
      case Some(e) => execute(e)
      case None    => editor.popStatus("No execution has been invoked yet.")
    }
  }

  @Fn("Visits the workspace's execution configuration file.")
  def workspaceEditExecutions () {
    pspace.execs.visitConfig(editor)
  }

  @Fn("Describes the current project.")
  def describeProject () {
    project.visitDescription(editor)
  }

  // TODO: move this into workspace mode, have ProjectSpace participate in describe calldowns
  @Fn("Displays summary info for all projects in this workspace.")
  def describeWorkspace () {
    val bb = new BufferBuilder(view.width()-1)
    pspace.describeSelf(bb)
    val bname = s"*workspace*"
    editor.visitBuffer(bb.applyTo(editor.bufferConfig(bname).reuse().mode("help").create()))
  }

  @Fn("Adds the current project to the current workspace.")
  def addToWorkspace () {
    pspace.addProject(project)
    editor.popStatus(s"'${project.name}' added to '${pspace.name}' workspace.")
  }

  @Fn("Removes the current project from the current workspace.")
  def removeFromWorkspace () {
    pspace.removeProject(project)
    editor.popStatus(s"'${project.name}' removed from '${pspace.name}' workspace.")
  }

  //
  // Implementation details

  private def maybeShowTestOutput () =
    if (config(showOutputOnTest)) editor.visitBuffer(project.tester.buffer(editor))

  private def execute (exec :Execution) {
    pspace.execs.execute(editor, exec)
    config(lastExecution).update(exec)
  }
}
