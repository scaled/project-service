//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Doc, Element, Kind, Ref, Relation, Sig}
import codex.store.ProjectStore
import scaled._
import scaled.util.BufferBuilder
import scaled.code.CodeConfig

object CodexUtil {
  import DocFormatterPlugin.Format

  /** Returns the CSS style to use for `kind`. */
  def styleFor (kind :Kind) = kind match {
    case Kind.MODULE => Some(CodeConfig.moduleStyle)
    case Kind.TYPE   => Some(CodeConfig.typeStyle)
    case Kind.FUNC   => Some(CodeConfig.functionStyle)
    case Kind.VALUE  => Some(CodeConfig.variableStyle)
    case _           => None
  }

  /** Formats the supplied signature with colorizations and such. */
  def formatSig (sig :Sig, indent :String) :Seq[LineV] = {
    val lines = Seq.builder[LineV]()
    Line.onLines(sig.text) { (l, lstart) =>
      val len = l.length
      val lb = Line.builder(indent + l)
      for (el <- sig.uses) {
        val off = el.offset - lstart
        if (off >= 0 && off < len) styleFor(el.kind) foreach {
          val start = indent.length+off ; val end = start+el.length
          s => lb.withStyle(s, start, end).withTag(el, start, end)
        }
      }
      lines += lb.build()
    }
    lines.build()
  }

  /** Resolves the documentation for `df`. If `df` has no documentation, this will search for
    * inherited documentation for any def which `df` `Relation.OVERRIDES`.
    */
  def resolveDoc (psvc :ProjectService, stores :Iterable[ProjectStore],
                  docr :DocReader, df :Def) :Format = {
    def refDoc (ref :Ref) = Option.from(Ref.resolve(stores, ref)) flatMap(findDoc)
    def relDoc (refs :Iterable[Ref]) :Option[Format] = {
      val iter = refs.iterator() ; while (iter.hasNext) {
        val doc = refDoc(iter.next)
        if (doc.isDefined) return doc
      }
      None
    }
    def findDoc (df :Def) :Option[Format] = Option.from(df.doc) match {
      case Some(doc) =>
        val docf = psvc.docFormatter(df.source.fileExt)
        Some(docf.format(df, doc, docr.resolve(df.source, doc)))
      case None => relDoc(df.relations(Relation.OVERRIDES))
    }
    findDoc(df) getOrElse DocFormatterPlugin.NoDoc
  }

  /** Formats doc and signature information into a buffer builder and returns it. */
  def summarizeDef (env :Env, stores :Iterable[ProjectStore], df :Def) :BufferBuilder = {
    val bb = new BufferBuilder(env.view.width()-2)
    val fmt = resolveDoc(env.msvc.service[ProjectService], stores, new DocReader(), df)
    try fmt.full("", bb)
    catch {
      case e :Exception => bb.add(Line.fromText(e.toString))
    }
    df.sig.ifPresent(new java.util.function.Consumer[Sig]() {
      def accept (sig :Sig) = bb.add(formatSig(sig, ""))
    })
    bb
  }

  /** Creates a popup for `df` including sig and docs at `loc`. */
  def mkDefPopup (env :Env, stores :Iterable[ProjectStore], df :Def, loc :Loc) :Popup = {
    val bb = summarizeDef(env, stores, df)
    if (bb.lines.isEmpty) bb.add(s"No docs or sig for '${df.name}'")
    Popup.lines(bb.lines, Popup.UpRight(loc))
  }

  /** Creates a debug popup for `df` at `loc`. */
  def mkDebugPopup (df :Def, loc :Loc) :Popup = {
    def safeGet (thunk : => Any) = try thunk.toString catch { case t :Throwable => t.toString }
    val text = SeqBuffer[String]()
    text += s"ID:    ${df.idToString}"
    text += s"Outer: ${df.outerIdToString}"
    text += s"Kind:  ${df.kind}"
    text += s"Exp:   ${df.exported}"
    text += s"Name:  ${df.name}"
    text += s"Off:   ${df.offset}"
    text += s"Body:  ${df.bodyStart}:${df.bodyEnd}"
    text += s"Src:   ${safeGet(df.source)}"
    text += s"GID:   ${safeGet(df.globalRef)}"
    Popup.text(text, Popup.UpRight(loc))
  }
}
