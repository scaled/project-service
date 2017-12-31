//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import java.util.HashMap
import org.eclipse.lsp4j._
import scaled._

class LangCompiler (p :Project) extends Compiler(p) {
  import Compiler._

  val diagsByStore = new HashMap[Store, Iterable[Note]]()

  def gotDiagnostics (store :Store, diags :Iterable[Diagnostic]) {
    diagsByStore.put(store, diags map toNote(store))
    // now update the diagnostics with this store first (TODO: order the remainder?)
    def flattenDiags (error :Boolean) = {
      val diags = Seq.builder[Note]()
      diags ++= diagsByStore.get(store).filter(_.isError == error)
      for (ss <- diagsByStore.keySet ; if (ss != store)) {
        diags ++= diagsByStore.get(ss).filter(_.isError == error)
      }
      diags.build()
    }
    gotStatus(flattenDiags(true), flattenDiags(false))
  }

  override def describeEngine = "langserver"
  override def recompileOnSave = false
  override protected def compile (buffer :Buffer, file :Option[Path]) =
    Future.success(true) // TODO: use langserver commands?
  override protected def nextNote (buffer :Buffer, start :Loc) = NoMoreNotes

  private def isError (diag :Diagnostic) = diag.getSeverity == DiagnosticSeverity.Error

  private def toNote (store :Store)(diag :Diagnostic) = {
    val lines = Seq.from(diag.getMessage.split(System.lineSeparator))
    Note(store, LSP.fromPos(diag.getRange.getStart), lines, isError(diag))
  }
}
