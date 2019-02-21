//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import java.util.Collections
import org.eclipse.lsp4j._
import scaled._

class LangIntel (client :LangClient, project :Project) extends Intel {
  import Intel._

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
      import org.eclipse.lsp4j.jsonrpc.messages.Either
      val contents = if (hover == null) null else hover.getContents
      if (contents == null || (contents.isLeft && contents.getLeft.isEmpty))
        view.window.popStatus("No info available.")
      else {
        val buffer = Buffer.scratch("*popup*")
        val wrapWidth = view.width()-4
        LSP.toScala(contents) match {
          case Left(segs) => for (seg <- segs) client.format(buffer, wrapWidth, seg)
          case Right(markup) => client.format(buffer, wrapWidth, markup)
        }
        view.popup() = Popup.buffer(buffer, Popup.UpRight(view.point()))
      }
    })
  }

  override def enclosers (view :RBufferView, loc :Loc) :Seq[Defn] = {
    abort("TODO")
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

  override def renameElementAt (view :RBufferView, loc :Loc, newName :String) =
    client.serverCaps.flatMap(caps => {
      val canRename = Option(caps.getRenameProvider).map(LSP.toScala).map(_ match {
        case Left(bv) => bv.booleanValue
        case Right(opts) => true
      }) || false
      if (!canRename) abort("Language Server does not support rename refactoring.")

      val rparams = new RenameParams(LSP.docId(view.buffer), LSP.toPos(loc), newName)
      LSP.adapt(textSvc.rename(rparams), view.window.exec).map(edits => {
        val docChanges = edits.getDocumentChanges
        if (docChanges != null) {
          println(s"TODO(docChanges): $docChanges")
        }

        // TODO: resource changes...

        val changes = edits.getChanges
        if (changes == null) abort(s"No changes returned for rename (to $newName)")
        // def toEdit (edit :TextEdit) = Edit(LSP.fromRange(edit.getRange), edit.getNewText)
        Map.view(changes).map((uri, edits) => new Renamer(LSP.toStore(uri)) {
          def validate (buffer :Buffer) {} // LSP does not supply enough info to validate
          def apply (buffer :Buffer) = for (edit <- edits) buffer.replace(
            LSP.fromRange(edit.getRange), Seq(Line(edit.getNewText)))
        })
      })
    })
}
