//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.util.Date
import reactual.{Future, Value, ValueV}
import scala.annotation.tailrec
import scaled._

/** Static [[Compiler]] stuffs. */
object Compiler {

  /** Represents a compilation error extracted from a buffer.
    * @param path the path to the compilation unit to which the error refers
    * @param loc the location of the error in that compilation unit
    * @param descrip the description of the error provided by the compiler
    */
  case class Error (path :String, loc :Loc, descrip :String)
}

/** Provides an interface whereby project mode can initiate project compilation and display
  * compiler feedback in the appropriate buffers.
  */
abstract class Compiler (project :Project) extends AutoCloseable {
  import Compiler._

  /** Indicates the number of errors in the most recent compile run. This is set to -1 while a
    * compile is in progress. */
  def errCount :ValueV[Int] = _errCount

  /** Returns the buffer in which we record compiler output. It will be created if needed. */
  def buffer (editor :Editor) :Buffer = editor.createBuffer(
    s"*compile-${project.name}*", true, ModeInfo("log" /*project-compile*/, Nil)).buffer

  /** Initiates a recompilation of this project, if supported.
    * @return a future which will report a summary of the compilation, or a failure if compilation
    * is not supported by this project.
    */
  def recompile (editor :Editor, interactive :Boolean) {
    val buf = buffer(editor)
    val start = System.currentTimeMillis
    buf.replace(buf.start, buf.end, Line.fromTextNL(s"Compilation started at ${new Date}..."))
    _errCount() = -1
    compile(buf).onFailure(editor.emitError).onSuccess { success =>
      // scan the results buffer for compiler errors
      val errs = Seq.newBuilder[Error]
      @inline @tailrec def loop (loc :Loc) :Unit = nextError(buf, loc) match {
        case Some((err, next)) => errs += err ; loop(next)
        case None => // done!
      }
      loop(buf.start)
      _currentErr = -1
      _errs = errs.result
      _errCount() = _errs.size
      val duration = System.currentTimeMillis - start
      val durstr = if (duration < 1000) s"$duration ms" else s"${duration / 1000} s"
      buf.append(Line.fromTextNL(s"Completed in $durstr, at ${new Date}."))
      // report feedback to the user if this was requested interactively
      if (interactive) {
        val result = if (success) "succeeded" else "failed"
        val msg = s"Compilation $result with ${_errCount()} error(s)."
        editor.emitStatus(msg)
      }
    }
    if (interactive) editor.emitStatus("Recompile initiated...")
  }

  /** Advances the internal error pointer to the next error and visits that buffer in `editor`.
    * If we are at the end of the list, the user is informed via feedback that we have reached the
    * last error, and the internal counter is reset so that a subsequent request to visit the next
    * error will visit the first error.
    */
  def visitNextError (editor :Editor) {
    if (_errs.isEmpty) editor.popStatus("No compilation errors.")
    else {
      _currentErr += 1
      if (_currentErr < _errs.length) visitError(editor, _errs(_currentErr))
      else {
        _currentErr = -1
        editor.emitStatus("At last error. Repeat command to start from first error.")
      }
    }
  }

  /** Regresses the internal error pointer to the previous error and visits that buffer in `editor`.
    * If we are at the start of the list, the user is informed via feedback that we have reached the
    * first error, and the internal counter is reset so that a subsequent request to visit the
    * previous error will visit the last error.
    */
  def visitPrevError (editor :Editor) {
    if (_errs.isEmpty) editor.popStatus("No compilation errors.")
    else if (_currentErr == -1) {
      _currentErr = _errs.length-1
      visitError(editor, _errs(_currentErr))
    } else if (_currentErr == 0) {
      _currentErr = -1
      editor.emitStatus("At first error. Repeat command to start from last error.")
    } else {
      _currentErr -= 1
      visitError(editor, _errs(_currentErr))
    }
  }

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

  private def visitError (editor :Editor, err :Compiler.Error) {
    editor.visitFile(Store(err.path)).point() = err.loc
    editor.popStatus(err.descrip)
  }

  private[this] var _errs = Seq[Error]()
  private[this] var _currentErr = -1
  private[this] val _errCount = Value(0)
}
