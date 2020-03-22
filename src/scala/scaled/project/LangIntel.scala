//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Kind, Flavor}
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

  override def describeElement (view :RBufferView) :Unit = {
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

  override def enclosers (view :RBufferView, loc :Loc) = {
    val sparams = new DocumentSymbolParams(LSP.docId(view.buffer))
    LSP.adapt(textSvc.documentSymbol(sparams), view.window.exec).map {
      case null => Seq()
      case syms if (syms.isEmpty) => Seq()
      case syms =>
        // convert from wonky "list of eithers" to two separate lists; we'll only have one kind of
        // symbol or the other but lsp4j's "translation" of LSP's "type" is a minor disaster
        val sis = SeqBuffer[Symbol]()
        val dss = SeqBuffer[DocumentSymbol]()
        syms map(LSP.toScala) foreach {
          case Left(si) => sis += si
          case Right(ds) => dss += ds
        }

        if (!dss.isEmpty) {
          def loopDS (dss :SeqV[DocumentSymbol], encs :List[DocumentSymbol]) :Seq[Defn] =
            dss.find(ds => LSP.fromRange(ds.getRange).contains(loc)) match {
              case Some(ds) => loopDS(Seq.view(ds.getChildren), ds :: encs)
              case None => encs.map(toDefn(view.buffer, _)).toSeq
            }
          loopDS(dss, Nil)
        }
        // if we have sym infos then we don't have body ranges; so we just find the closest symbol
        // that starts before our location and for which the next symbol ends after our location,
        // then use 'container name' to reconstruct encloser chain...
        else if (!sis.isEmpty) {
          def symloc (si :Symbol) = LSP.fromPos(si.getLocation.getRange.getStart)
          val (before, after) = sis.partition(si => symloc(si) <= loc)
          if (before.isEmpty) Seq()
          else {
            def outers (si :Symbol, encs :List[Symbol]) :Seq[Defn] =
              before.find(_.getName == si.getContainerName) match {
                case Some(osi) => outers(osi, si :: encs)
                case None => (si :: encs).reverse.map(toDefn(view.buffer, _)).toSeq
              }
            outers(before.last, Nil)
          }
        }
        else Seq()
    }
  }

  override def visitElement (view :RBufferView, target :Window) :Future[Boolean] = {
    val pparams = LSP.toTDPP(view.buffer, view.point())
    LSP.adapt(textSvc.definition(pparams), target.exec).map(res => LSP.toScala(res) match {
      case Left(locs) => locs.find(_.getUri != null).map(LSP.URILoc.apply)
      case Right(links) => links.find(_.getTargetUri != null).map(LSP.URILoc.apply)
    }).onSuccess(_ match {
      case None      => view.window.popStatus(s"Unable to locate definition.")
      case Some(loc) => client.visitLocation(project, loc.name, loc, target)
    }).map(_.isDefined)
  }

  override def visitSymbol (sym :SymbolInformation, target :Window) = client.visitLocation(
    project, s"${sym.getName}:${sym.getContainerName}", LSP.URILoc(sym.getLocation), target)

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
          def validate (buffer :Buffer) :Unit = {} // LSP does not supply enough info to validate
          def apply (buffer :Buffer) = {
            val backToFront = Seq.view(edits).sortBy(e => LSP.fromPos(e.getRange.getStart)).reverse
            for (edit <- backToFront) buffer.replace(
              LSP.fromRange(edit.getRange), Seq(Line(edit.getNewText)))
          }
        })
      })
    })

  private def toKF (kind :SymbolKind) = kind match {
    case SymbolKind.Array => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Boolean => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Class => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.Constant => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Constructor => (Kind.FUNC, Flavor.NONE)
    case SymbolKind.Enum => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.EnumMember => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Event => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.Field => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.File => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Function => (Kind.FUNC, Flavor.NONE)
    case SymbolKind.Interface => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.Key => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Method => (Kind.FUNC, Flavor.NONE)
    case SymbolKind.Module => (Kind.MODULE, Flavor.NONE)
    case SymbolKind.Namespace => (Kind.MODULE, Flavor.NONE)
    case SymbolKind.Null => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Number => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Object => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Operator => (Kind.FUNC, Flavor.NONE)
    case SymbolKind.Package => (Kind.MODULE, Flavor.NONE)
    case SymbolKind.Property => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.String => (Kind.VALUE, Flavor.NONE)
    case SymbolKind.Struct => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.TypeParameter => (Kind.TYPE, Flavor.NONE)
    case SymbolKind.Variable => (Kind.VALUE, Flavor.NONE)
    case _ => (Kind.VALUE, Flavor.NONE)
  }

  // TODO: convert Codex to use row/char instead of offset & avoid need to convert here
  private def toDefn (b :BufferV, sym :Symbol) = {
    val r = LSP.fromRange(sym.getLocation.getRange)
    val (kind, flavor) = toKF(sym.getKind)
    Defn(kind, flavor, sym.getName(), None, b.offset(r.start), b.offset(r.start), b.offset(r.end))
  }

  private def toDefn (b :BufferV, sym :DocumentSymbol) = {
    val r = LSP.fromRange(sym.getRange) ; val sr = LSP.fromRange(sym.getSelectionRange)
    val (kind, flavor) = toKF(sym.getKind)
    Defn(kind, flavor, sym.getName(), Option(sym.getDetail),
         b.offset(sr.start), b.offset(r.start), b.offset(r.end))
  }
}
