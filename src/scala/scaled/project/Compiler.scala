//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.util.Date
import scala.annotation.tailrec
import scaled._
import scaled.util.BufferBuilder

/** Static [[Compiler]] stuffs. */
object Compiler {

  /** Defines various project status states. */
  sealed abstract class Status (val glyph :String) {
    def indicator :String = s" $glyph"
    def tip :String = s"$glyph = $toString"
  }
  object Unknown extends Status("?") {
    override def indicator = ""
    override def toString = "project has not yet been compiled"
  }
  object Compiling extends Status("\u231B") { // hourglass
    override def toString = "project is currently compiling"
  }
  object NoErrors extends Status("\u263A") { // smiley
    override def toString = "project has no compile errors"
  }
  case class Errors (count :Int) extends Status("\u2639") { // frownz!
    override def indicator = s" $glyph $count"
    override def toString = s"project has $count compile error(s)"
  }

  /** Configures a compile. See [[Compiler.compile]].
    * @param tests if true, the tests companion project will be compiled if this compilation
    * succeeds.
    * @param incremental if true, do an incremental compile, otherwise do a full compile.
    * @param interactive whether the compile was initiated interactively; controls feedback
    * reporting.
    */
  case class Config (tests :Boolean, incremental :Boolean, interactive :Boolean)

  /** Creates a [[Visit]] for the supplied compiler error. */
  def errorVisit (path :String, loc :Loc, descrip :Seq[String]) :Visit = new Visit() {
    override protected def go (window :Window) = {
      val view = window.focus.visitFile(Store(path))
      view.point() = loc
      // TODO: use different kind of popup that has an arrow pointing to loc and otherwise adjust
      // its position up or down, left or right to fit yet still point to loc
      view.showPopup(Popup.text(descrip, Popup.UpRight(loc)).toError)
    }
  }
}

/** Provides an interface whereby project mode can initiate project compilation and display
  * compiler feedback in the appropriate buffers.
  */
abstract class Compiler (project :Project) extends AutoCloseable {
  import Compiler._

  /** Appends compiler status to our modeline status string and tooltip.
    * @param sb the builder for the status line
    * @param tb the builder for the tooltip */
  def addStatus (sb :StringBuilder, tb :StringBuilder) {
    val s = _status()
    sb.append(s.indicator)
    tb.append("\n").append(s.tip)
  }

  /** Adds compiler info to the project info buffer. */
  def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Compiler")
    bb.addKeyValue("Status: ", _status().toString)
  }

  /** Returns the buffer in which we record compiler output. It will be created if needed. */
  def buffer () :Buffer = project.createBuffer(s"*compile:${project.name}*", "log")

  /** Initiates a compilation of this project's source code, if supported.
    * @return a future which will report a summary of the compilation, or a failure if compilation
    * is not supported by this project.
    */
  def compile (window :Window, config :Config) {
    val buf = buffer()
    val start = System.currentTimeMillis
    buf.replace(buf.start, buf.end, Line.fromTextNL(s"Compilation started at ${new Date}..."))
    _status() = Compiling
    compile(buf, config.incremental).onFailure(window.emitError).onSuccess { success =>
      // scan the results buffer for compiler errors
      val ebuf = Seq.builder[Visit]
      @inline @tailrec def unfold (loc :Loc) :Unit = nextError(buf, loc) match {
        case Some((err, next)) => ebuf += err ; unfold(next)
        case None => // done!
      }
      unfold(buf.start)

      val errs = ebuf.build()
      window.visits() = new Visit.List("compile error", errs)
      _status() = errs.size match {
        case 0 => NoErrors
        case n => Errors(n)
      }

      val duration = System.currentTimeMillis - start
      val durstr = if (duration < 1000) s"$duration ms" else s"${duration / 1000} s"
      buf.append(Line.fromTextNL(s"Completed in $durstr, at ${new Date}."))
      // report feedback to the user if this was requested interactively
      if (config.interactive) {
        val result = if (success) "succeeded" else "failed"
        val msg = s"Compilation $result with ${errs.size} error(s)."
        window.emitStatus(msg)
      }

      if (success && config.tests) {
        project.testCompanion.foreach { _.compiler.compile(window, config.copy(tests=false)) }
      }
    }
    if (config.interactive) window.emitStatus(s"${project.name} recompiling...")
  }

  /** Requests that the build artifacts be deleted so that the next recompilation processes all
    * source files in the project.
    */
  def clean () {} // nada by default

  /** Requests that this compiler be reset. If a connection to an external compiler is being
    * maintained, it should be closed so that the next [[recompile]] request causes a new compiler
    * to be started.
    */
  def reset () {} // nada by default

  /** Called when this compiler is no longer needed. This should terminate any external processes
    * and release any resources retained by this instance. */
  def close () {} // nada by default

  /** Initiates a compilation, sends output to `buffer`, returns a future that indicates compilation
    * success or failure. */
  protected def compile (buffer :Buffer, incremental :Boolean) :Future[Boolean]

  /** Scans `buffer` from `start` to find the next error.
    *
    * @return a tuple containing a [[Visit]] which will visit the next error (see
    * [[Compiler.errorVisit]]), and the location at which to search for subsequent errors, or
    * `None` if no next error was found.
    */
  protected def nextError (buffer :Buffer, start :Loc) :Option[(Visit,Loc)]

  private[this] val _status = Value[Status](Unknown)
  _status onEmit { project.updateStatus() }
}
