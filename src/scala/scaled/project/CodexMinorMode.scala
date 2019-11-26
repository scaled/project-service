//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import scaled._
import scaled.util.{Chars}

/** An abstract base class for minor modes which aim to make use of Codex services. The base class
  * takes care of obtaining a reference to the `ProjectSpace` and `Project` as well as the
  * `SourceIndex` for the file. It also defines a number of useful methods for identifying the
  * element at the point and operating on it.
  */
abstract class CodexMinorMode (env :Env) extends MinorMode(env) {

  protected def codex = Codex(buffer)
  protected def project = Project(buffer)
  protected def index = buffer.state[SourceIndex]
  protected def reqIndex = index getOrElse abort("No Codex index available for this file.")

  protected def codexRead (prompt :String, kind :Kind)(fn :JConsumer[Def]) :Unit =
    window.mini.read(prompt, wordAt(view.point()), history(kind),
                     codex.completer(project, kind)).onSuccess(fn)

  protected def codexVisit (prompt :String, kind :Kind) :Unit = codexRead(prompt, kind)(visit)

  protected def codexSummarize (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(summarize)

  protected def visit (df :Def) :Unit = {
    window.visitStack.push(view) // push current loc to the visit stack
    codex.visit(window, df)
  }

  protected def summarize (df :Def) :Unit = {
    CodexSummaryMode.visitDef(wspace.getInfoWindow("codex"), df)
  }

  protected def onElemAt (loc :Loc)(fn :(Element, Loc, Def) => Unit) :Unit =
    codex.onElemAt(buffer, loc)(fn)

  protected def onEncloser (loc :Loc)(fn :JConsumer[Def]) :Unit =
    reqIndex.encloser(buffer.offset(loc)) match {
      case None => abort("Could not find enclosing type.")
      case Some(df) => fn.accept(df)
    }

  /** Returns the "word" at the specified location in the buffer. */
  protected def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString

  protected def history (kind :Kind) = kind match {
    case Kind.MODULE => wspace.historyRing("codex-module")
    case Kind.TYPE   => wspace.historyRing("codex-type")
    case Kind.FUNC   => wspace.historyRing("codex-func")
    case Kind.VALUE  => wspace.historyRing("codex-value")
    case _           => wspace.historyRing("codex-other")
  }
}
