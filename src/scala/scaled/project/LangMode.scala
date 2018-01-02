//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.net.URI
import java.util.{List => JList}
import org.eclipse.lsp4j._
import scaled._
import scaled.grammar.GrammarService
import scaled.major.ReadingMode
import scaled.util.{Chars, Errors, Filler, Process, FuzzyMatch}

@Minor(name="lang", tags=Array("project"), stateTypes=Array(classOf[LangClient]),
       desc="""A minor mode that provides generic Language Server fns.""")
class LangMode (env :Env, major :ReadingMode) extends MinorMode(env) {
  val langClient = LangClient(buffer)
  def textSvc = langClient.server.getTextDocumentService

  note(langClient.messages.onValue { msg =>
    window.emitStatus(s"${msg.getType}: ${msg.getMessage}")
  })

  override def keymap = super.keymap.
    bind("show-type-at-point", "M-t", "C-c C-d").
    bind("visit-symbol", "C-c C-k").
    bind("visit-element", "M-.");

  //
  // FNs

  @Fn("Describes the type at the point.")
  def showTypeAtPoint () {
    val pparams = toTDPP(view.point())
    LSP.adapt(textSvc.hover(pparams), window).onSuccess(hover => {
      val contents = hover.getContents
      if (contents.isEmpty) window.popStatus("No info available.")
      else {
        val buffer = Buffer.scratch("*popup*")
        val wrapWidth = view.width()-4
        Seq.view(contents).foldLeft(buffer)((b, c) => langClient.format(buffer, wrapWidth, c))
        view.popup() = Popup.buffer(buffer, Popup.UpRight(view.point()))
      }
    })
  }

  @Fn("Navigates to the referent of the element at the point.")
  def visitElement () {
    val pparams = toTDPP(view.point())
    LSP.adapt(textSvc.definition(pparams), window).onSuccess(_.find(_.getUri != null) match {
      case None => window.popStatus(s"Unable to locate definition.")
      case Some(loc) =>
        window.visitStack.push(env.view) // push current loc to the visit stack
        langClient.visitLocation(LSP.getName(loc), loc, window)
    })
  }

  @Fn("Queries for a project-wide symbol and visits it.")
  def visitSymbol () {
    window.mini.read("Symbol:", wordAt(view.point()), symbolHistory,
                     langClient.symbolCompleter(window)).onSuccess(visit)
  }

  private def visit (sym :SymbolInformation) {
    window.visitStack.push(view) // push current loc to the visit stack
    langClient.visitSymbol(sym, window)
  }

  private def symbolHistory = wspace.historyRing("langserver-symbol")

  private def toTDPP (pos :Loc) = LSP.toTDPP(buffer, pos)

  private def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
