//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind}
import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.util.{BufferBuilder, Chars, Errors}

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {

  @Var("If true, the project will be automatically recompiled on file save.")
  val recompileOnSave = key(true)

  @Var("""If true, any test companion project will be automatically recompiled after
          a successful main project recompile.""")
  val recompileTests = key(true)

  @Var("If true, the test output buffer will be shown when tests are run interactively.")
  val showOutputOnTest = key(true)

  /** Provides the CSS style for `note`. */
  def noteStyle (note :Intel.Note) = note.sev match {
    case Intel.Hint    => "noteHintFace"
    case Intel.Info    => "noteInfoFace"
    case Intel.Warning => "noteWarningFace"
    case Intel.Error   => "noteErrorFace"
  }

  def isNoteStyle (style :String) = style startsWith "note"
}

/** A minor mode which provides fns for interacting with project files and services.
  *
  * Some stock key bindings are also redirected toward project-centric versions, for example
  * `C-x C-f` is rerouted to `find-file-in-project`. Where possible, the original fns are exposed
  * via slightly varied key bindings.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="project", tags=Array("project"), stateTypes=Array(classOf[Project]),
       desc="""A minor mode that provides project-centric fns.""")
class ProjectMode (env :Env) extends MinorMode(env) {
  val project = Project(buffer)
  import project.pspace
  import ProjectConfig._

  override def configDefs = ProjectConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/project.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("describe-project",  "C-h p").

    // file fns
    bind("find-file-in-project",      "C-x C-p").
    bind("find-file-in-test-project", "C-x S-C-p").
    bind("find-file-other-project",   "C-x C-o").

    // intel fns
    bind("describe-element", "C-c C-d").
    bind("visit-element",    "M-.").
    bind("visit-symbol",     "C-c C-k").
    bind("rename-element",   "C-c C-r").

    // compilation fns
    bind("compile-incremental", "F5").
    bind("compile-full",        "S-F5").

    // warning navigation fns
    bind("visit-next-warning", "C-S-]").
    bind("visit-prev-warning", "C-S-[").

    // test fns
    bind("run-all-tests",     "C-c C-t C-a").
    bind("run-file-tests",    "C-c C-t C-f").
    bind("run-test-at-point", "C-c C-t C-p").
    bind("repeat-last-test",  "C-c C-t C-r", "F6").
    bind("visit-tests",       "C-x C-t").

    // execution fns
    bind("workspace-execute",       "C-c C-e").
    bind("workspace-execute-again", "C-c C-a");

  //
  // Behaviors

  /** Finds a file in `proj` and visits it. */
  private def findFileIn (proj :Project) :Unit = {
    window.mini.read(
      s"Find file in project (${proj.name}):", "", proj.fileHistory, proj.files.completer
    ) map(wspace.openBuffer) onSuccess(frame.visit(_))
  }

  private def bufferNotes = project.notes(buffer.store)

  // provides a custom Visit.List that cycles through the notes in the current buffer and only
  // advances to the next buffer if we have no notes in this buffer
  private def notesVisitList (notes :SeqV[Intel.Note]) :Visit.List =
    new Visit.List("buffer note", notes) {
      override def next (win :Window) :Unit = if (isEmpty) skip(win,  1) else super.next(win)
      override def prev (win :Window) :Unit = if (isEmpty) skip(win, -1) else super.prev(win)
      private def skip (win :Window, delta :Int) = {
        val noteStores = project.noteStores
        if (noteStores.isEmpty) if (delta > 0) super.next(win) else super.prev(win)
        else {
          val storeIdx = noteStores.indexOf(buffer.store)
          val skipIdx = (storeIdx + delta + noteStores.length) % noteStores.length
          val skipList = notesVisitList(project.notes(noteStores(skipIdx))())
          if (delta > 0) skipList.next(win) else skipList.prev(win)
        }
      }
    }

  private def updateVisits (onCreate :Boolean)(list :Visit.List) :Unit = {
    val curlist = window.visits()
    // we only want to update the visit list on buffer creation if we're not currently visiting
    // something else or if we're currently visiting the same kind of thing
    if (!onCreate || curlist.isEmpty || curlist.thing == list.thing)
      window.visits() = list
  }

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => s._2)))

  // forward project feedback to our window
  note(project.feedback.onValue(_ fold((window.emitStatus _).tupled, window.emitError)))

  // trigger a recompile on buffer save, if thusly configured
  note(buffer.storeV onEmit {
    if (config(recompileOnSave) && project.compiler.recompileOnSave) compile(true, false)
  })

  // when new analysis notes or compiler errors are generated, stuff them into the visit list
  note(project.compiler.errors onValue updateVisits(false))
  note(bufferNotes.onValue(notes => {
    // clear all note styles from the buffer and readd to the current set; this is not very
    // efficient but tracking the old notes through all possible buffer edits is rather a PITA
    buffer.removeTags(classOf[String], isNoteStyle, buffer.start, buffer.end)
    for (n <- notes) buffer.addStyle(noteStyle(n), buffer.clamp(n.region))
    updateVisits(false)(notesVisitList(notes));
  }))

  // when first visiting this buffer, maybe visit analysis notes or compiler errors
  if (!bufferNotes().isEmpty) updateVisits(true)(notesVisitList(bufferNotes()))
  else if (!project.compiler.errors().isEmpty) updateVisits(true)(project.compiler.errors())

  //
  // General FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def findFileInProject () :Unit = findFileIn(project)

  @Fn("""Like find-file-in-project except it only searches in the test companion project.
         If no test companion project exists, this project is searched.""")
  def findFileInTestProject () :Unit = findFileIn(project.testCompanion || project)

  @Fn("""Reads a project name from the minibuffer, then reads a file from that project (with smart
         completion), and visits it.""")
  def findFileOtherProject () :Unit = {
    val pcomp = Completer.from(pspace.allProjects)(_._2)
    window.mini.read(s"Project:", "", projectHistory, pcomp) onSuccess { case pt =>
      findFileIn(pspace.projectFor(pt._1))
    }
  }

  //
  // Intel FNs

  @Fn("Describes the element at the point.")
  def describeElement () :Unit = Intel(buffer).describeElement(view)

  @Fn("Navigates to the referent of the element at the point.")
  def visitElement () :Unit = {
    val loc = view.point()
    Intel(buffer).visitElement(view, window).onSuccess { visited =>
      if (visited) window.visitStack.push(buffer, loc)
    }
  }

  @Fn("Queries for a project-wide symbol and visits it.")
  def visitSymbol () :Unit = {
    val intel = Intel(buffer)
    window.mini.read("Symbol:", wordAt(view.point()), symbolHistory,
                     intel.symbolCompleter(None)).onSuccess(sym => {
      window.visitStack.push(view) // push current loc to the visit stack
      intel.visitSymbol(sym, window)
    })
  }

  @Fn("Renames all occurrences of the element at the point.")
  def renameElement () :Unit = {
    val loc = view.point()
    val intel = Intel(buffer)
    window.mini.read("New name:", wordAt(loc), renameHistory, Completer.none).
      flatMap(name => intel.renameElementAt(view, loc, name)).
      onSuccess(renamers => {
        println(s"Renames $renamers")
        if (renamers.isEmpty) abort(
          "No renames returned for refactor. Is there an element at the point?")

        def doit (save :Boolean) = try {
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.validate(buffer)
          }
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.apply(buffer)
            if (save) buffer.save()
          }
        } catch {
          case err :Throwable => window.exec.handleError(err)
        }

        // if there are occurrences outside the current buffer, confirm the rename
        if (renamers.size == 1 && renamers(0).store == view.buffer.store) doit(false)
        else window.mini.readYN(
          s"'Element occurs in ${renamers.size-1} source file(s) not including this one. " +
            "Undoing the rename will not be trivial, continue?").onSuccess { yes =>
          if (yes) doit(true)
        }
      }).
      onFailure(window.exec.handleError)
  }

  @Fn("Restarts the language server client for the active project.")
  def restartLangClient () :Unit = {
    project.emitStatus("Restaring langserver client...")
    project.lang.restartClient(buffer);
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
  def resetCompiler () :Unit = project.compiler.reset()

  @Fn("Displays the internal status of the compiler. For debugging.")
  def showCompilerStatus () :Unit = {
    project.compiler.getStatus(project.logBuffer)
    showCompilerOutput(true)
  }

  @Fn("Displays the buffer that contains compiler output for this project.")
  def showCompilerOutput () :Unit = showCompilerOutput(true)

  @Fn("Navigates to the next warning in the current compiler warning list, if any.")
  def visitNextWarning () :Unit = project.compiler.warnings().next(window)

  @Fn("Navigates to the previous warning in the current compiler warning list, if any.")
  def visitPrevWarning () :Unit = project.compiler.warnings().prev(window)

  //
  // Test FNs

  @Fn("""Runs all of this project's tests. Output is displayed in a buffer named *test{project}*.
         Failures identified in said output are placed in the visit list and can be navigated
         using `visit-next` and `visit-prev`.""")
  def runAllTests () :Unit = runTest { _ =>
    if (!tester.runAllTests(window, true)) abort(s"No tests found in ${project.name}.")
    maybeShowTestOutput()
  }

  @Fn("""Identifies the test file associated with the current buffer (which may be the buffer's
         file itself if that file contains tests) and runs the tests in it.
         See project-run-all-tests for info on test output and failure navigation.""")
  def runFileTests () :Unit = tester.findTestFile(bufferFile) match {
    case None        => abort(s"Cannot find test file for $bufferFile")
    case Some(tfile) => runTest { _ =>
      if (!tester.runTests(window, true, tfile)) abort(s"No tests found in $tfile.")
      maybeShowTestOutput()
    }
  }

  @Fn("Determines the test method enclosing the point and runs it.")
  def runTestAtPoint () :Unit = {
    Intel(buffer).enclosers(view, view.point()).map(tester.findTestFunc).onSuccess(_ match {
      case Some(defn) =>
        println(s"Tester ${project.tester} on ${defn.name}")
        project.tester.runTest(window, bufferFile, defn).onFailure(window.exec.handleError)
               .onSuccess { _ =>
          // display the test output as a popup over the point
          view.popup() = Popup.lines(project.logBuffer.lines, Popup.UpRight(view.point()))
        }
      case None => abort("Unable to find enclosing test function.")
    }).onFailure(window.exec.handleError)
  }

  @Fn("""Repeats the last run-all-tests or run-file-tests, in the window it was run.
         If no test has been run in this project, all tests are run.""")
  def repeatLastTest () :Unit = tester.lastTest.getOption.foreach(_.apply(view))

  @Fn("Visits the source file that defines tests for the file in the current buffer.")
  def visitTests () :Unit = {
    val file = bufferFile
    tester.findTestFile(file) match {
      case None => window.popStatus(
        s"Unable to find test source for ${project.root.path.relativize(file)}.")
      case Some(tfile) => window.focus.visitFile(Store(tfile))
    }
  }

  @Fn("""Forcibly aborts any tests in progress and terminates any daemon currently being used
         to run this project's tests.""")
  def abortTests () :Unit = tester.abort()

  @Fn("Displays the buffer that contains test output for this project.")
  def showTestOutput () :Unit = showTestOutput(true)

  @Fn("""Resets the tester for this project. If an external process is handling testing, it will
         be terminated so that a new process is started to handle the next test command.""")
  def resetTester () :Unit = tester.reset()

  //
  // Execute FNs

  @Fn("Invokes a particular execution in this workspace.")
  def workspaceExecute () :Unit = {
    val exns = pspace.execs.executions
    if (exns.isEmpty) window.popStatus(s"${pspace.name} defines no executions.")
    else window.mini.read(s"Execute:", "", pspace.execHistory,
                          Completer.from(exns)(_.name)) onSuccess execute
  }

  @Fn("""Reinvokes the last invoked execution.""")
  def workspaceExecuteAgain () :Unit = {
    wspace.state[Execution].getOption match {
      case Some(e) => execute(e)
      case None    => window.popStatus("No execution has been invoked yet.")
    }
  }

  @Fn("Visits the workspace's execution configuration file.")
  def workspaceEditExecutions () :Unit = {
    pspace.execs.visitConfig(window)
  }

  //
  // Meta FNs

  @Fn("Describes the current project.")
  def describeProject () :Unit = {
    project.visitDescription(window)
  }

  @Fn("Adds the current project to the current workspace.")
  def addToWorkspace () :Unit = {
    pspace.addProject(project)
    window.popStatus(s"'${project.name}' added to '${pspace.name}' workspace.")
  }

  @Fn("Removes the current project from the current workspace.")
  def removeFromWorkspace () :Unit = {
    pspace.removeProject(project)
    window.popStatus(s"'${project.name}' removed from '${pspace.name}' workspace.")
  }

  @Fn("Removes a project from the current workspace.")
  def removeProject () :Unit = {
    val comp = Completer.from(pspace.allProjects)(_._2)
    window.mini.read(s"Project:", "", projectHistory, comp) onSuccess(info => {
      val (root, name) = info
      pspace.removeProject(pspace.projectFor(root))
      window.popStatus(s"Removed '$name' from '${pspace.name}' workspace.")
    })
  }

  //
  // Implementation details

  private def projectHistory = wspace.historyRing("project-name")
  private def symbolHistory = wspace.historyRing("project-symbol")
  private def renameHistory = wspace.historyRing("project-rename")

  private def bufferFile :Path = buffer.store.file getOrElse { abort(
      "This buffer has no associated file. A file is needed to detect tests.") }

  private def tester = (project.testCompanion || project).tester
  private def showTestOutput (focus :Boolean) =
    wspace.getInfoWindow("tests").focus.visit(tester.resultsBuffer, focus)
  private def maybeShowTestOutput () = if (config(showOutputOnTest)) showTestOutput(false)

  private def compile (incremental :Boolean, interactive :Boolean) :Unit = {
    val cfg = Compiler.Config(config(recompileTests), interactive,
                              if (incremental) buffer.store.file else None)
    project.compiler.compile(window, cfg)
  }
  private def showCompilerOutput (focus :Boolean) =
    wspace.getInfoWindow("compile").focus.visit(project.logBuffer, focus)

  private def runTest (action :RBufferView => Unit) :Unit = {
    tester.lastTest() = action
    action(view)
  }

  private def execute (exec :Execution) :Unit = {
    pspace.execs.execute(exec, project)
    // track our last execution in the workspace state
    wspace.state[Execution]() = exec
  }

  private def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
