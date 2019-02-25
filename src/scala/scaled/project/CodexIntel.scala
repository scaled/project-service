//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind, Source}
import scaled._

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

  override def enclosers (view :RBufferView, loc :Loc) = {
    def toDefns (df :Def, defns :List[Defn]) :Seq[Defn] =
      if (df == null) Seq() ++ defns.reverse else toDefns(df.outer, toDefn(df) :: defns)
    Future.success((for (index <- view.buffer.state[SourceIndex].getOption ;
                         edef <- index.encloser(view.buffer.offset(loc)))
                    yield toDefns(edef, Nil)) getOrElse Seq())
  }

  override def visitElement (view :RBufferView, target :Window) :Future[Boolean] = {
    codex.onElemAt(view.buffer, view.point())((_, _, df) => codex.visit(target, df))
    Future.success(true)
  }

  override def visitSymbol (sym :Def, target :Window) = codex.visit(target, sym)

  override def renameElementAt (view :RBufferView, loc :Loc, newName :String) =
    codex.onElemAt(view.buffer, loc) { (elem, loc, df) =>
      if (df.kind != Kind.FUNC && df.kind != Kind.VALUE) abort(
        "Rename only supported for methods and variables. Not types or packages.")
      if (newName == df.name) abort("Name unchanged, nothing to be done.")

      val origM = Matcher.exact(df.name)
      val nameL = Line(newName)
      def mkRenamer (src :Source, useOffs :Array[Int]) = new Renamer(Codex.toStore(src)) {
        // if this src contains our def, add it to the list of rename locs
        val offs = if (src == df.source) df.offset +: useOffs else useOffs
        // convert the offsets to locs and sort them in reverse order
        // (that way when we replace them, the earlier locs don't mess up the later locs)
        def locs (buffer :Buffer) = (offs map buffer.loc).sortWith(_ > _).toSeq
        def validate (buffer :Buffer) = locs(buffer) foreach { loc =>
          if (!buffer.line(loc).matches(origM, loc.col)) abort(
            s"${store.name} not in sync with intel @ $loc. Can't rename.")
        }
        def apply (buffer :Buffer) = locs(buffer) foreach { loc =>
          buffer.replace(loc, df.name.length, nameL)
        }
      }
      Future.success(Map.view(codex.store(project).usesOf(df)).map(mkRenamer))
    }

  private def toDefn (df :Def) = Defn(df.kind, df.flavor, df.name, Option.from(df.sig).map(_.text),
                                      df.offset, df.bodyStart, df.bodyEnd)
}
