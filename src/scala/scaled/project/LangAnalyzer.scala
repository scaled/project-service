//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import java.util.HashMap
import org.eclipse.lsp4j._
import scaled._
import scaled.util.BufferBuilder

class LangAnalyzer (project :Project, client :LangClient) extends Analyzer {
  import Analyzer._

  type Symbol = SymbolInformation
  def textSvc = client.server.getTextDocumentService
  def wspaceSvc = client.server.getWorkspaceService
  val notesByStore = new HashMap[Store, Iterable[Note]]()

  def gotDiagnostics (pdp :PublishDiagnosticsParams) {
    val store = LSP.toStore(pdp.getUri)
    val diags = pdp.getDiagnostics
    notesByStore.put(store, diags map(diag => Note(
      store,
      Region(LSP.fromPos(diag.getRange.getStart), LSP.fromPos(diag.getRange.getEnd)),
      diag.getMessage,
      diag.getSeverity match {
        case DiagnosticSeverity.Hint => Hint
        case DiagnosticSeverity.Information => Info
        case DiagnosticSeverity.Warning => Warning
        case DiagnosticSeverity.Error => Error
      })))

    // now update the diagnostics with this store first (TODO: order the remainder?)
    project.notes.update({
      val nb = Seq.builder[Note]()
      nb ++= notesByStore.get(store)
      for (ss <- notesByStore.keySet ; if (ss != store)) nb ++= notesByStore.get(ss)
      nb.build()
    })
  }

  override def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Analyzer")
    bb.addKeysValues("Engine:" -> "langserver",
                     "Status: " -> "?")
  }

  override def symbolCompleter (kind :Option[Kind]) = new Completer[SymbolInformation] {
    override def minPrefix = 2
    def complete (glob :String) =
      LSP.adapt(wspaceSvc.symbol(new WorkspaceSymbolParams(glob)), project.exec).
      // TODO: filter results by kind if a kind is provided
      map(results => Completion(glob, results, false)(client.formatSym))
  }

  override def fqName (sym :SymbolInformation) :String = client.fqName(sym)

  override def describeElement (view :RBufferView) {
    val pparams = LSP.toTDPP(view.buffer, view.point())
    LSP.adapt(textSvc.hover(pparams), view.window.exec).onSuccess(hover => {
      val contents = hover.getContents
      if (contents.isEmpty) view.window.popStatus("No info available.")
      else {
        val buffer = Buffer.scratch("*popup*")
        val wrapWidth = view.width()-4
        Seq.view(contents).foldLeft(buffer)((b, c) => client.format(buffer, wrapWidth, c))
        view.popup() = Popup.buffer(buffer, Popup.UpRight(view.point()))
      }
    })
  }

  override def visitElement (view :RBufferView, target :Window) :Future[Boolean] = {
    val pparams = LSP.toTDPP(view.buffer, view.point())
    LSP.adapt(textSvc.definition(pparams), target.exec).map(_.find(_.getUri != null)).
      onSuccess(_ match {
        case None      => view.window.popStatus(s"Unable to locate definition.")
        case Some(loc) => client.visitLocation(LSP.getName(loc), loc, target)
      }).
      map(_.isDefined)
  }

  override def visitSymbol (sym :SymbolInformation, target :Window) =
    client.visitSymbol(sym, target)
}
