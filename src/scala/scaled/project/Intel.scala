//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import java.net.URI
import java.nio.file.Path
import scaled._
import scaled.util.Errors

/** Static [[Intel]] bits. */
object Intel {

  /** Returns the `Intel` associated with `buffer`. */
  def apply (buffer :Buffer) :Intel = buffer.state.get[Intel].getOrElse {
    throw Errors.feedback(s"No intel configured in buffer: '$buffer'")
  }

  /** Enumerates different kinds of [[Note]]s. */
  sealed trait Severity
  /** A [[Node]] kind for hint messages. */
  object Hint extends Severity
  /** A [[Node]] kind for informational messages. */
  object Info extends Severity
  /** A [[Node]] kind for warning messages. */
  object Warning extends Severity
  /** A [[Node]] kind for error messages. */
  object Error extends Severity

  /** Describes information about a region of code in a file. */
  case class Note (store :Store, region :Region, msg :String, sev :Severity) extends Visit {

    /** Formats the message for display in a popup. */
    def format (maxWidth :Int, msg :String) :Buffer = {
      val wrapped = SeqBuffer[String]
      msg.split(System.getProperty("line.separator")) foreach { line =>
        if (line.length <= maxWidth) wrapped += line
        else line.grouped(maxWidth) foreach { wrapped += _ }
      }
      val buffer = Buffer.scratch("*popup*")
      buffer.append(wrapped.map(Line.apply))
      buffer
    }

    override protected def go (window :Window) = {
      val view = window.focus.visitFile(store)
      view.point() = region.start
      val pop = Popup.buffer(format(view.width()-2, msg), Popup.UpRight(region.start))
      view.showPopup(if (sev == Error) pop.toError else pop)
    }
  }

  /** Handles a rename refactoring in a single store. */
  abstract class Renamer (val store :Store) {
    /** Validates that this renamer can be applied to `buffer` (checking that its renames match
      * up with the buffer as expected).
      * @throw FeedbackException if something doesn't line up.
      */
    def validate (buffer :Buffer) :Unit
    /** Applies the rename to `buffer` (which will correspond to this renamer's `store`). */
    def apply (buffer :Buffer) :Unit
  }
}

/** Integrates with a language's compiler to provide more sophisticated feedback and analysis. This
  * includes compilation notes (warnings, errors, etc.) as well as providing metadata based on the
  * (simple) model used by [[Codex]]. This should be added to buffer state by a project component
  * if intelligence is available for the buffer's file.
  */
abstract class Intel {
  import Intel._

  type Symbol

  /** A completer on all symbols known to the analyzer (project or workspace wide).
    * @param kind an optional kind to which to restrict the results. */
  def symbolCompleter (kind :Option[Kind]) :Completer[Symbol]

  /** Returns the fully qualified name of this symbol. */
  def fqName (sym :Symbol) :String

  /** Describes the element at `view`'s point. The results should be displayed in a popup. */
  def describeElement (view :RBufferView) :Unit

  // /** Summarizes the element at the point. The results should be displayed in a buffer. */
  // def summarizeElement (window :Window, view :BufferView) :Unit

  /** Visits the element at `view`'s point, in `target`.
    * @return true if an element was visited, false if no element could be discerned at the current
    * point. */
  def visitElement (view :RBufferView, target :Window) :Future[Boolean]

  /** Visits the supplied symbol (obtained from the symbol completer), in `target`. */
  def visitSymbol (sym :Symbol, target :Window) :Unit

  /** Requests that the symbol at `loc` be renamed to `newName`. */
  def renameElementAt (view :RBufferView, window :Window, loc :Loc,
                       newName :String) :Future[Seq[Renamer]]

  protected def abort (msg :String) :Nothing = throw Errors.feedback(msg)
}
