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
  def buffer (editor :Editor) :Buffer = editor.bufferConfig(s"*compile:${project.name}*").
    mode("log" /*project-compile*/).tags("project").state(project.asState).reuse().create().buffer

  /** The latest compilation errors in a navigable ring. */
  def errors :ErrorRing = _errs

  /** Initiates a recompilation of this project, if supported.
    * @return a future which will report a summary of the compilation, or a failure if compilation
    * is not supported by this project.
    */
  def recompile (editor :Editor, interactive :Boolean) {
    val buf = buffer(editor)
    val start = System.currentTimeMillis
    buf.replace(buf.start, buf.end, Line.fromTextNL(s"Compilation started at ${new Date}..."))
    _status() = Compiling
    compile(buf).onFailure(editor.emitError).onSuccess { success =>
      // scan the results buffer for compiler errors
      val errs = Seq.builder[Error]
      @inline @tailrec def loop (loc :Loc) :Unit = nextError(buf, loc) match {
        case Some((err, next)) => errs += err ; loop(next)
        case None => // done!
      }
      loop(buf.start)
      _errs = errorRing(errs.build())
      _status() = _errs.count match {
        case 0 => NoErrors
        case n => Errors(n)
      }
      val duration = System.currentTimeMillis - start
      val durstr = if (duration < 1000) s"$duration ms" else s"${duration / 1000} s"
      buf.append(Line.fromTextNL(s"Completed in $durstr, at ${new Date}."))
      // report feedback to the user if this was requested interactively
      if (interactive) {
        val result = if (success) "succeeded" else "failed"
        val msg = s"Compilation $result with ${_errs.count} error(s)."
        editor.emitStatus(msg)
      }
    }
    if (interactive) editor.emitStatus("Recompile initiated...")
  }

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
  protected def compile (buffer :Buffer) :Future[Boolean]

  /** Scans `buffer` from `start` to find the next error.
    *
    * @return a tuple containing an error, and the location at which to search for subsequent
    * errors, or `None` if no next error was found.
    */
  protected def nextError (buffer :Buffer, start :Loc) :Option[(Error,Loc)]

  private def errorRing (errs :Seq[Error]) = new ErrorRing("error", errs) {
    override def onNone = "No compilation errors."
  }

  private[this] var _errs = errorRing(Seq())
  private[this] val _status = Value[Status](Unknown)
  _status onEmit { project.updateStatus() }
}
