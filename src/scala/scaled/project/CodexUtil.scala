//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Doc, Element, Sig}
import scaled._
import scaled.util.BufferBuilder

object CodexUtil {

  /** Creates a popup for `df` including sig and docs at `loc`. */
  def mkDefPopup (env :Env, df :Def, loc :Loc) :Popup = {
    val bb = new BufferBuilder(env.view.width()-2)
    val fmt = env.msvc.service[ProjectService].docFormatter(df.source.fileExt)
    df.doc.ifPresent(new java.util.function.Consumer[Doc]() {
      def accept (doc :Doc) :Unit = try {
        val r = df.source().reader()
        val buf = new Array[Char](doc.length)
        r.skip(doc.offset)
        r.read(buf)
        r.close()
        fmt.format(df, doc, new String(buf)).full("", bb)
      } catch {
        case e :Exception => bb.add(Line.fromText(e.toString))
      }
    })
    df.sig.ifPresent(new java.util.function.Consumer[Sig]() {
      def accept (sig :Sig) = bb.add(CodexSummaryMode.formatSig(sig, ""))
    })
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
    text += s"Src:   ${safeGet(df.source)}"
    text += s"GID:   ${safeGet(df.globalRef)}"
    Popup.text(text, Popup.UpRight(loc))
  }

}
