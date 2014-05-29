//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._

/** Represents an error associated with a source location. Used by [[Compiler]] and [[Tester]].
  * @param path the path to the compilation unit to which the error refers
  * @param loc the location of the error in that compilation unit
  * @param descrip the description show to the user when the error is visited
  */
case class Error (path :String, loc :Loc, descrip :Seq[String])

/** Contains a list of errors and allows the user to visit them in turn, maintaining an internal
  * pointer which wraps around when either end is reached.
  */
class ErrorRing (thing :String, errs :Seq[Error]) {

  private[this] var _current = -1

  /** Returns the number of errors in the ring. */
  def count = errs.size

  /** Advances the internal error pointer to the next error and visits that buffer in `editor`.
    * If we are at the end of the list, the user is informed via feedback that we have reached the
    * last error, and the internal counter is reset so that a subsequent request to visit the next
    * error will visit the first error.
    */
  def visitNext (editor :Editor) {
    if (errs.isEmpty) editor.popStatus(onNone)
    else {
      _current += 1
      if (_current < count) visitError(editor, errs(_current))
      else {
        _current = -1
        editor.emitStatus(atLast)
      }
    }
  }

  /** Regresses the internal error pointer to the previous error and visits that buffer in `editor`.
    * If we are at the start of the list, the user is informed via feedback that we have reached the
    * first error, and the internal counter is reset so that a subsequent request to visit the
    * previous error will visit the last error.
    */
  def visitPrev (editor :Editor) {
    if (errs.isEmpty) editor.popStatus(onNone)
    else if (_current == -1) {
      _current = count-1
      visitError(editor, errs(_current))
    } else if (_current == 0) {
      _current = -1
      editor.emitStatus(atFirst)
    } else {
      _current -= 1
      visitError(editor, errs(_current))
    }
  }

  private def visitError (editor :Editor, err :Error) {
    val view = editor.visitFile(Store(err.path))
    view.point() = err.loc
    // TODO: use different kind of popup that has an arrow pointing to loc and otherwise adjust
    // its position up or down, left or right to fit yet still point to loc
    view.showPopup(Popup(err.descrip, Popup.UpRight(err.loc), true, true))
  }

  protected def onNone  = s"No ${thing}s."
  protected def atFirst = s"At first $thing. Repeat command to start from last $thing."
  protected def atLast  = s"At last $thing. Repeat command to start from first $thing."
}
