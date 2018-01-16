//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import org.eclipse.lsp4j._
import scaled._

class LangAnalyzer (client :LangClient, project :Project) extends Analyzer {
  import Analyzer._

  type Symbol = SymbolInformation
  def textSvc = client.server.getTextDocumentService
  def wspaceSvc = client.server.getWorkspaceService

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
        case Some(loc) => client.visitLocation(project, LSP.getName(loc), loc, target)
      }).
      map(_.isDefined)
  }

  override def visitSymbol (sym :SymbolInformation, target :Window) = client.visitLocation(
    project, s"${sym.getName}:${sym.getContainerName}", sym.getLocation, target)
}
