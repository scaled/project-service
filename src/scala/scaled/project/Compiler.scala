//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Path, Paths}
import java.util.Date
import scala.annotation.tailrec
import scaled._
import scaled.util.BufferBuilder

/** Provides an interface whereby project mode can initiate project compilation and display
  * compiler feedback in the appropriate buffers.
  */
abstract class Compiler (val project :Project) extends Project.Component {
  import Compiler._

  /** The current set of compiler warnings, if any. */
  val warnings = Value[Visit.List](null)

  /** The current set of compiler errors, if any. */
  val errors = Value[Visit.List](null)

  /** A signal emitted when an individual file is successfully recompiled. */
  val compiled = Signal[Path]()

  // initialize warnings & errors to empty lists
  setNotes(Seq(), Seq())

  /** Appends compiler status to our modeline status string and tooltip.
    * @param sb the builder for the status line
    * @param tb the builder for the tooltip */
  def addStatus (sb :StringBuilder, tb :StringBuilder) {
    val s = _status()
    sb.append(s.indicator)
    tb.append("\n").append(s.tip)
  }

  /** Adds compiler info to the project info buffer. */
  override def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Compiler")
    bb.addKeysValues("Engine:" -> describeEngine,
                     "Status: " -> _status().toString)
    bb.addSection("Options:")
    describeOptions(bb)
  }

  /** Describes the underlying compiler used by this compiler. */
  def describeEngine :String

  /** Adds info on compiler options to the project info buffer. */
  def describeOptions (bb :BufferBuilder) {
    // nothing by default
  }

  /** Queries the compiler for internal status and appends it to `buffer`. */
  def getStatus (buffer :Buffer) {
    buffer.append(Line.fromTextNL(s"Status not supported by this compiler ($describeEngine)."))
  }

  /** Indicates whether this compiler should be invoked when a buffer is saved. */
  def recompileOnSave :Boolean = true

  /** Initiates a compilation of this project's source code, if supported.
    * @return a future which will report a summary of the compilation, or a failure if compilation
    * is not supported by this project.
    */
  def compile (window :Window, config :Config) {
    val buf = project.logBuffer
    val start = System.currentTimeMillis
    buf.replace(buf.start, buf.end, Line.fromTextNL(s"Compiling ${project.name} at ${new Date}..."))
    _status() = Compiling
    compile(buf, config.file).via(window.exec.ui).
      onFailure(window.emitError).
      onSuccess { success =>
        // scan the results buffer for compiler errors
        val wbuf = Seq.builder[Note]
        val ebuf = Seq.builder[Note]
        @inline @tailrec def unfold (loc :Loc) :Unit = nextNote(buf, loc) match {
          case NoMoreNotes  => // done!
          case NoteLoc(note, next) => (if (note.isError) ebuf else wbuf) += note ; unfold(next)
        }
        unfold(buf.start)

        val ecount = ebuf.size ; val wcount = wbuf.size
        gotStatus(ebuf.build(), wbuf.build())

        val duration = System.currentTimeMillis - start
        val durstr = if (duration < 1000) s"$duration ms" else s"${duration / 1000} s"
        buf.append(Line.fromTextNL(s"Completed in $durstr, at ${new Date}."))
        // report feedback to the user if this was requested interactively
        if (config.interactive) {
          val result = if (success) "succeeded" else "failed"
          window.emitStatus(s"${project.name} compile $result: " +
            s"${ecount} error(s), ${wcount} warning(s).")
        }

        if (success) config.file.foreach { path => compiled.emit(path) }

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

  /** Initiates a compilation, sends output to `buffer`.
    * @param file the file that triggered this compile on save, or `None` for full recompile.
    * @return a future that indicates compilation success or failure. */
  protected def compile (buffer :Buffer, file :Option[Path]) :Future[Boolean]

  /** Scans `buffer` from `start` to find the next compiler note.
    * @return the next note found in the buffer and the position at which to seek further notes,
    * or `NoMoreNotes` if nothing more was found. */
  protected def nextNote (buffer :Buffer, start :Loc) :NoteLoc = NoMoreNotes

  protected def gotStatus (errs :SeqV[Note], warns :SeqV[Note]) {
    _status() = if (warns.isEmpty && errs.isEmpty) NoProblems else Problems(errs.size, warns.size)
    setNotes(errs, warns)
  }

  private def setNotes (errs :SeqV[Note], warns :SeqV[Note]) {
    warnings() = new Visit.List("compile warning", warns)
    errors()   = new Visit.List("compile error", errs)
  }

  private[this] val _status = Value[Status](Unknown)
  _status onEmit { project.updateStatus() }
}

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
  object NoProblems extends Status("\u263A") { // smiley
    override def toString = "project has no compile errors or warnings"
  }
  case class Problems (errors :Int, warnings :Int) extends Status("\u2639") { // frownz!
    override def indicator = s" e$errors w$warnings"
    override def toString = s"project has $errors error(s) and $warnings warning(s)"
  }

  /** Configures a compile. See [[Compiler.compile]].
    * @param tests if true, the tests companion project will be compiled if this compilation
    * succeeds.
    * @param interactive whether the compile was initiated interactively; controls feedback
    * reporting.
    * @param file the source file that triggered this compile by being saved, if any, which will
    * result in an incremental recompile, or `None` for a full recompile.
    */
  case class Config (tests :Boolean, interactive :Boolean, file :Option[Path])

  /** Encapsulates a compiler warning or error. */
  case class Note (file :Store, loc :Loc, descrip :SeqV[String], isError :Boolean) extends Visit {
    override protected def go (window :Window) = {
      val view = window.focus.visitFile(file)
      view.point() = loc
      val maxWidth = view.width()-2
      val wrapped = if (!descrip.exists(_.length > maxWidth)) descrip
      else {
        val wbuf = Seq.builder[String]
        for (line <- descrip) {
          if (line.length <= maxWidth) wbuf += line
          else for (seg <- line.grouped(maxWidth)) wbuf += seg
        }
        wbuf.build
      }
      val pop = Popup.text(wrapped, Popup.UpRight(loc))
      view.showPopup(if (isError) pop.toError else pop)
    }
  }

  /** Used by [[Compiler.nextNote]]. */
  case class NoteLoc (note :Note, next :Loc)

  /** A sentinel tuple instance indicating that note parsing is done. */
  val NoMoreNotes = NoteLoc(null :Note, Loc.Zero)
}
