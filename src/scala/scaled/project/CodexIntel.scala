//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind}
import scaled._
import scaled.util.Errors

/** Provides some code intelligence based on `Codex`. */
class CodexIntel (codex :Codex, project :Project) extends Intel {
  import Intel._
  type Symbol = Def

  override def symbolCompleter (kind :Option[Kind]) :Completer[Def] =
    codex.completer(project, kind || Kind.TYPE)

  override def fqName (sym :Def) = sym.fqName

  override def describeElement (view :RBufferView) {
    codex.onElemAt(view.buffer, view.point()) { (elem, loc, df) =>
      view.popup() = codex.mkDefPopup(view, codex.stores(project), df, loc)
    }
  }

  override def visitElement (view :RBufferView, target :Window) :Future[Boolean] = {
    codex.onElemAt(view.buffer, view.point())((_, _, df) => codex.visit(target, df))
    Future.success(true)
  }

  override def visitSymbol (sym :Def, target :Window) = codex.visit(target, sym)
}
