//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind}
import java.nio.file.Path
import javafx.scene.control.Tooltip
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.util.{BufferBuilder, Errors}

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {

  @Var("If true, the project will be automatically recompiled on file save.")
  val recompileOnSave = key(true)

  @Var("""If true, any test companion project will be automatically recompiled after
          a successful main project recompile.""")
  val recompileTests = key(true)

  @Var("If true, the test output buffer will be shown when tests are run interactively.")
  val showOutputOnTest = key(false)

  @Var("""If non-empty, the geometry of a window to open the first time an execution is performed.
          Output will be shown in this window. Geometry is of the form 'WxH+X+Y'.""")
  var execWindowGeom = key("")
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
class ProjectMode (env :Env) extends CodexMinorMode(env) {
  import ProjectConfig._
  import project.pspace

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => new Tooltip(s._2))))

  override def configDefs = ProjectConfig :: super.configDefs
  override def keymap = super.keymap.
    bind("describe-project",  "C-h p").
    bind("describe-projects", "C-h S-p").

    // file fns
    bind("find-file-in-project",    "C-x C-p").
    bind("find-file-other-project", "C-x C-o").

    // compilation fns
    bind("compile-incremental", "F5").
    bind("compile-full",        "S-F5").

    // test fns
    bind("run-all-tests",     "C-c C-t C-a").
    bind("run-file-tests",    "C-c C-t C-f").
    bind("run-test-at-point", "C-c C-t C-p").
    bind("run-closest-test",  "C-c C-t C-t").
    bind("repeat-last-test",  "C-c C-t C-r", "F6").
    bind("visit-tests",       "C-x C-t").

    // execution fns
    bind("workspace-execute",       "C-c C-e").
    bind("workspace-execute-again", "C-c C-a");

  /** Finds a file in `proj` and visits it. */
  def findFileIn (proj :Project) {
    window.mini.read(
      s"Find file in project (${proj.name}):", "", proj.fileHistory, proj.fileCompleter
    ) map(wspace.openBuffer) onSuccess frame.visit
  }

  //
  // Behaviors

  // trigger a recompile on buffer save, if thusly configured
  note(buffer.storeV onEmit {
    if (config(recompileOnSave)) compile(true, false)
  })

  // when new compiler errors are generated, always stuff them into the visit list
  note(project.compiler.errors onValue updateErrors(true))
  // but when we first visit this buffer, only stuff them into the visit list if we're not already
  // visiting some other list (or already this project's errors)
  updateErrors(false)(project.compiler.errors.getOption)

  private def updateErrors (force :Boolean)(errsOpt :Option[Visit.List]) = errsOpt match {
    case None => // nada
    case Some(errs) =>
      // only switch to our project's errors if we're not currently visiting something else
      val (thing, visits) = window.visits match {
        case OptValue(list) => (list.thing, list.visits)
        case _              => (errs.thing, Seq())
      }
      if (force || (thing == errs.thing && (visits ne errs.visits))) window.visits() = errs
  }

  //
  // General FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def findFileInProject () :Unit = findFileIn(project)

  @Fn("""Reads a project name from the minibuffer, then reads a file from that project (with smart
         completion), and visits it.""")
  def findFileOtherProject () {
    val pcomp = Completer.from(pspace.allProjects)(_._2)
    window.mini.read(s"Project:", "", projectHistory, pcomp) onSuccess { case pt =>
      findFileIn(pspace.reqProjectIn(pt._1))
    }
  }

  //
  // Compile FNs

  @Fn("""Initiates an incremntal compilation of the current project. Output is displayed in a
         buffer named *compile:{project}*. Errors are placed in the visit list and can be
         navigated using `visit-next` and `visit-prev`.""")
  def compileIncremental () :Unit = compile(true, true)

  @Fn("""Initiates an full compilation of the current project. Output is displayed in a buffer
         named *compile:{project}*. Errors are placed in the visit list and can be navigated
         using `visit-next` and `visit-prev`.""")
  def compileFull () :Unit = compile(false, true)

  @Fn("""Resets the compiler for this project. This can be useful if the compiler misbehaves,
         due perhaps to a command line compiler stomping on its files or something similar.""")
  def resetCompiler () {
    project.compiler.reset()
  }

  @Fn("Displays the buffer that contains compiler output for this project.")
  def showCompilerOutput () {
    frame.visit(project.compiler.buffer())
  }

  //
  // Test FNs

  @Fn("""Runs all of this project's tests. Output is displayed in a buffer named *test{project}*.
         Failures identified in said output are placed in the visit list and can be navigated
         using `visit-next` and `visit-prev`.""")
  def runAllTests () :Unit = runTest { _ =>
    if (!tester.runAllTests(window, true)) abort(s"No tests found in ${project.name}.")
    maybeShowTestOutput(window)
  }

  @Fn("""Identifies the test file associated with the current buffer (which may be the buffer's file
         itself if that file contains tests) and runs the tests in it.
         See project-run-all-tests for info on test output and failure navigation.""")
  def runFileTests () :Unit = tester.findTestFile(bufferFile) match {
    case None        => abort(s"Cannot find test file for $bufferFile")
    case Some(tfile) => runTest { _ =>
      if (!tester.runTests(window, true, tfile, Seq())) abort(s"No tests found in $tfile.")
      maybeShowTestOutput(window)
    }
  }

  @Fn("Determines the test method enclosing the point and runs it.")
  def runTestAtPoint () {
    onEncloser(view.point()) { df =>
      def ffunc (df :Def) :Def =
        if (df == null) abort("Unable to find enclosing test function.")
        else if (tester.isTestFunc(df)) df
        else ffunc(df.outer)
      val tfunc = ffunc(df)
      runTest { view =>
        project.tester.runTest(window, bufferFile, tfunc).onSuccess { _ =>
          // display the test output as a popup over the point
          view.popup() = Popup(tester.buffer().lines, Popup.UpRight(view.point()), true, false)
        }
      }
    }
  }

  @Fn("""Attempts to run-test-at-point but falls back to run-file-tests if no tests can be
         found at the point and run-all-tests if run-file-tests fails.""")
  def runClosestTest () {
    try runTestAtPoint()
    catch {
      case fe :Errors.FeedbackException =>
        window.emitStatus(fe.getMessage)
        try runFileTests()
        catch {
          case fe :Errors.FeedbackException =>
            window.emitStatus(fe.getMessage)
            runAllTests()
        }
    }
  }

  @Fn("""Repeats the last run-all-tests or run-file-tests, in the window it was run.
         If no test has been run in this project, all tests are run.""")
  def repeatLastTest () :Unit = tester.lastTest.getOption.foreach(_.apply(view))

  @Fn("Visits the source file that defines tests for the file in the current buffer.")
  def visitTests () {
    val file = bufferFile
    tester.findTestFile(file) match {
      case None => window.popStatus(
        s"Unable to find test source for ${project.root.path.relativize(file)}.")
      case Some(tfile) => window.focus.visitFile(Store(tfile))
    }
  }

  @Fn("""Forcibly aborts any tests in progress and terminates any daemon currently being used
         to run this project's tests.""")
  def abortTests () {
    tester.abort()
  }

  @Fn("Displays the buffer that contains test output for this project.")
  def showTestOutput () {
    window.focus.visit(tester.buffer())
  }

  //
  // Execute FNs

  @Fn("Invokes a particular execution in this workspace.")
  def workspaceExecute () {
    val exns = pspace.execs.executions
    if (exns.isEmpty) window.popStatus(s"${pspace.name} defines no executions.")
    else window.mini.read(s"Execute:", "", pspace.execHistory,
                          Completer.from(exns)(_.name)) onSuccess execute
  }

  @Fn("""Reinvokes the last invoked execution.""")
  def workspaceExecuteAgain () {
    wspace.state[Execution].getOption match {
      case Some(e) => execute(e)
      case None    => window.popStatus("No execution has been invoked yet.")
    }
  }

  @Fn("Visits the workspace's execution configuration file.")
  def workspaceEditExecutions () {
    pspace.execs.visitConfig(window)
  }

  //
  // Meta FNs

  // TODO: move this into workspace mode, have ProjectSpace participate in describe calldowns
  @Fn("Displays summary info for all projects in this workspace.")
  def describeProjects () {
    val bb = new BufferBuilder(view.width()-1)
    pspace.describeSelf(bb)
    window.focus.visit(bb.applyTo(project.createBuffer(s"*${wspace.name}:projects*", "help")))
  }

  @Fn("Describes the current project.")
  def describeProject () {
    project.visitDescription(window)
  }

  @Fn("Adds the current project to the current workspace.")
  def addToWorkspace () {
    pspace.addProject(project)
    window.popStatus(s"'${project.name}' added to '${pspace.name}' workspace.")
  }

  @Fn("Removes the current project from the current workspace.")
  def removeFromWorkspace () {
    pspace.removeProject(project)
    window.popStatus(s"'${project.name}' removed from '${pspace.name}' workspace.")
  }

  //
  // Implementation details

  private def projectHistory = Workspace.historyRing(wspace, "project-name")

  private def bufferFile :Path = buffer.store.file getOrElse { abort(
      "This buffer has no associated file. A file is needed to detect tests.") }
  private def tester = (project.testCompanion || project).tester
  private def maybeShowTestOutput (win :Window) = if (config(showOutputOnTest)) {
    val tbuf = tester.buffer()
    // if our buffer is already in a frame, just to-front its window, otherwise display it in the
    // current window's focus
    win.workspace.windowForBuffer(tbuf) match {
      case Some(twin) => println(s"found $twin") ; twin.toFront()
      case None       => println(s"nope $win") ; win.focus.visit(tbuf) ; win.toFront()
    }
  }

  private def compile (incremental :Boolean, interactive :Boolean) {
    val cfg = Compiler.Config(config(recompileTests), incremental, interactive)
    project.compiler.compile(window, cfg)
  }

  private def runTest (action :RBufferView => Unit) {
    tester.lastTest() = action
    action(view)
  }

  private def execute (exec :Execution) {
    case class ExecWindow (window :Window)
    // figure out which window to use for our execution
    val execwin = wspace.state[ExecWindow].getOption getOrElse ExecWindow(maybeCreateExecWindow)
    pspace.execs.execute(execwin.window, exec, project)
    // track our last execution data in the workspace state
    wspace.state[Execution]() = exec
    wspace.state[ExecWindow]() = execwin
  }

  private def maybeCreateExecWindow = Geometry.apply(config(execWindowGeom)) match {
    case None            => window // use the default window
    case sg @ Some(geom) => // create a new window with this geometry
      val win = wspace.openWindow(sg)
      win.focus.visit(buffer)
      win
  }
}
