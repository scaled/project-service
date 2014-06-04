//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model._
import javafx.scene.control.Tooltip
import reactual.{Value, OptValue}
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.EditingMode
import scaled.util.BufferBuilder

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {
  import EditorConfig._

  @Var("If true, the project will be automatically recompiled on file save.")
  val recompileOnSave = key(true)

  @Var("If true, the test output buffer will be shown when tests are run interactively.")
  val showOutputOnTest = key(false)

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

  /** The most recent index for the buffer's source file, if any. */
  val index = OptValue[SourceIndex]()
  // if our store gets indexed, store it in `index`
  note(project.codex.indexed.onValue { idx => if (idx.store == buffer.store) index() = idx })
  // request that our store be indexed (which should eventually populate `index`)
  note(buffer.storeV.onValueNotify(project.codex.reindex))

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => new Tooltip(s._2))))

  override def configDefs = ProjectConfig :: super.configDefs
  override def keymap = Seq(
    "C-h p" -> "describe-project",

    // file fns
    "C-x C-f" -> "project-find-file",
    "C-x C-o" -> "project-find-file-other",

    // navigation fns
    "C-]"     -> "project-next-error-or-failure",
    "C-["     -> "project-prev-error-or-failure",

    // compilation fns
    "C-c C-r" -> "project-recompile",

    // test fns
    "C-c S-C-t" -> "project-run-all-tests",
    "C-c C-t"   -> "project-run-file-tests",

    // execution fns
    "C-c C-e"   -> "project-execute",
    "C-c C-a"   -> "project-execute-again",
    "C-c S-C-e" -> "project-execute-in",

    // codex fns
    "C-c C-v C-m" -> "codex-visit-module",
    "C-c C-v C-t" -> "codex-visit-type",
    "C-c C-v C-f" -> "codex-visit-func",
    "C-c C-v C-v" -> "codex-visit-value",

    "C-c C-d"     -> "codex-describe-element",
    "M-."         -> "codex-visit-element",
    "M-,"         -> "codex-visit-pop",

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

  /** Returns a completer on elements of `kind` in this project's Codex. */
  def codexCompleter (kind :Kind) :Completer[Def] = new Completer[Def]() {
    import scala.collection.JavaConversions._
    def complete (prefix :String) :Completion[Def] = prefix.split(":", 2) match {
      case Array(name, path) =>
        elemComp(project.codex.find(Codex.Query.name(name).kind(kind)) filter(
          e => Completer.startsWithI(path)(pathString(e))))
      case Array(name) =>
        elemComp(project.codex.find(Codex.Query.prefix(name).kind(kind)))
    }
    private def elemComp (es :Seq[Def]) = completion(es, elemToString)
    private def pathString (d :Def) = d.qualifier
    private val elemToString = (e :Def) => s"${e.name}:${pathString(e)}"
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
    val pcomp = Completer.from(psvc.knownProjects)(_._2)
    editor.miniRead(s"Project:", "", config(projectHistory), pcomp) onSuccess { case pt =>
      findFileIn(psvc.projectIn(pt._1))
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
  // Codex FNs

  @Fn("Queries for a module (completed by the project's Codex) and navigates to its definition.")
  def codexVisitModule () :Unit = codexVisit("Module:", Kind.MODULE)

  @Fn("Queries for a type (completed by the project's Codex) and navigates to its definition.")
  def codexVisitType () :Unit = codexVisit("Type:", Kind.TYPE)

  @Fn("Queries for a function (completed by the project's Codex) and navigates to its definition.")
  def codexVisitFunc () :Unit = codexVisit("Function:", Kind.FUNC)

  @Fn("Queries for a value (completed by the project's Codex) and navigates to its definition.")
  def codexVisitValue () :Unit = codexVisit("Value:", Kind.VALUE)

  @Fn("""Displays the documentation and signature for the element at the point, if it is known to
         the project's Codex.""")
  def codexDescribeElement () {
    onElemAt(view.point(), (elem, df) => view.popup() = mkDefPopup(elem, df))
  }

  @Fn("""Navigates to the referent of the elmeent at the point, if it is known to this project's
         Codex.""")
  def codexVisitElement () {
    onElemAt(view.point(), (_, df) => {
      project.codex.visitStack.push(this.view) // push current loc to the visit stack
      val view = editor.visitFile(ProjectCodex.toStore(df.source()))
      view.point() = view.buffer.loc(df.offset)
    })
  }

  @Fn("Pops to the last place `codex-visit-foo` was invoked.")
  def codexVisitPop () {
    project.codex.visitStack.pop(editor)
  }

  //
  // Implementation details

  private def codexVisit (prompt :String, kind :Kind) {
    val dflt = "" // TODO: sym at point
    val hist = project.codexHistory(kind)
    editor.miniRead(prompt, dflt, hist, codexCompleter(kind)) onSuccess { df =>
      val infOpt = project.codex.resolve(df.ref)
      if (!infOpt.isPresent) editor.popStatus(s"Unable to resolve $df?")
      else {
        project.codex.visitStack.push(this.view) // push current loc to the visit stack
        val info = infOpt.get
        val view = editor.visitFile(ProjectCodex.toStore(info.source))
        view.point() = view.buffer.loc(df.offset)
      }
    }
  }

  private def onElemAt (loc :Loc, fn :(Element, Def) => Unit) {
    index.getOption match {
      case None => editor.popStatus("No Codex index available for this file.")
      case Some(idx) => idx.elementAt(loc) match {
        case None => editor.popStatus("No element could be found at the point.")
        case Some(elem) =>
          val dopt = project.codex.resolve(elem.ref)
          if (!dopt.isPresent) editor.popStatus(s"Unable to resolve referent for ${elem.ref}")
          else fn(elem, dopt.get)
      }
    }
  }

  private def mkDefPopup (elem :Element, df :Def) :Popup = {
    val text = ArrayBuffer[String]()
    df.doc.ifPresent(new java.util.function.Consumer[Doc]() {
      def accept (doc :Doc) :Unit = try {
        val r = df.source().reader()
        val buf = new Array[Char](doc.length)
        r.skip(doc.offset)
        r.read(buf)
        r.close()
        // TODO: trim leading whitespace up to start.col
        text ++= new String(buf).split(System.lineSeparator)
      } catch {
        case e :Exception => text += e.toString
      }
    })
    df.sig.ifPresent(new java.util.function.Consumer[Sig]() {
      def accept (sig :Sig) {
        text ++= sig.text.split(System.lineSeparator)
        // TODO: use defs and uses to style text
      }
    })
    Popup(text, Popup.UpRight(buffer.loc(elem.offset)))
  }

  private def maybeShowTestOutput () =
    if (config(showOutputOnTest)) editor.visitBuffer(project.tester.buffer(editor))

  private def execute (project :Project, exec :Execution) {
    project.runner.execute(editor, exec)
    config(lastExecution).update(project -> exec)
  }
}
