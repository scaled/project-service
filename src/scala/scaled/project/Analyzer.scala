//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** Static [[Analyzer]] bits. */
object Analyzer {

  /** Enumerates different kinds of [[Note]]s. */
  sealed trait Kind
  /** A [[Node]] kind for informational messages. */
  object Info extends Kind
  /** A [[Node]] kind for warning messages. */
  object Warning extends Kind
  /** A [[Node]] kind for error messages. */
  object Error extends Kind

  /** Describes information about a region of code in a file. */
  case class Note (
    kind  :Kind,
    file  :Path,
    start :Int,
    end   :Int,
    mesg  :String)
}

/** Integrates with a language's compiler to provide more sophisticated feedback and analysis. This
  * includes compilation notes (warnings, errors, etc.) as well as providing metadata based on the
  * (simple) model used by [[Codex]].
  */
abstract class Analyzer extends AutoCloseable {
  import Analyzer._

  /** The current analysis notes. */
  def notes :ValueV[Seq[Note]] = _notes

  // /** Returns information for the code element at `loc` in `buffer`. */
  // def elementAt (buffer :Buffer, loc :Loc) :Future[Model.Element]

  // /** Returns the nearest enclosing `kind` around `loc` in `buffer`. */
  // def enclosing (buffer :Buffer, loc :Loc, kind :Model.Kind) :Future[Model.Element]

  /** Initiates analysis of the specified paths.
    * When the analysis is complete, [[notes]] will be updated with the results. */
  def analyze (window :Window, paths :Seq[Path]) {
    analyze(paths).onFailure(window.emitError).onSuccess(_notes.update)
  }

  // TODO: visitNextNote, visitPrevNote? showNotes?

  /** Called when this analyzer is no longer needed. This should terminate any external processes
    * and release any resources retained by this instance. */
  def close () {} // nada by default

  protected def analyze (paths :Seq[Path]) :Future[Seq[Note]]

  private val _notes = Value(Seq[Note]())
}
