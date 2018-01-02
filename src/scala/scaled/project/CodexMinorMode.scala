//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import scaled._
import scaled.util.{Chars}

/** An abstract base class for minor modes which aim to make use of Codex services. The base class
  * takes care of obtaining a reference to the `ProjectSpace` and `Project` as well as the
  * `SourceIndex` for the file. It also defines a number of useful methods for identifying the
  * element at the point and operating on it.
  */
abstract class CodexMinorMode (env :Env) extends MinorMode(env) {

  val codex = Codex(buffer)
  // TODO: it's possible that our buffer's file could change and become part of a new project;
  // do we really want to handle that crazy case?
  val project = Project(buffer)

  /** The most recent index for the buffer's source file, if any. */
  val index = OptValue[SourceIndex]()
  // if our store gets indexed, store it in `index`
  note(codex.indexed.onValue { idx => if (idx.store == buffer.store) index() = idx })

  protected def reqIndex = index getOrElse abort("No Codex index available for this file.")

  protected def codexRead (prompt :String, kind :Kind)(fn :JConsumer[Def]) :Unit =
    window.mini.read(prompt, wordAt(view.point()), history(kind),
                     codex.completer(window, project, kind)).onSuccess(fn)

  protected def codexVisit (prompt :String, kind :Kind) :Unit = codexRead(prompt, kind)(visit)

  protected def codexSummarize (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(summarize)

  protected def visit (df :Def) {
    window.visitStack.push(view) // push current loc to the visit stack
    codex.visit(window, df)
  }

  protected def summarize (df :Def) {
    val summaryWindow = Geometry.apply(summaryWindowGeom) match {
      case None => window
      case Some(geom) => (wspace.windows.find(_.geometry == geom) ||
                          wspace.openWindow(Some(geom)))
    }
    CodexSummaryMode.visitDef(summaryWindow, df)
  }

  // allows a concrete codex minor mode to indicate that summaries should be presented in a
  // separate window with the specified geometry
  protected def summaryWindowGeom :String = ""
  // (TODO: this notion of 'show aux data in a separate window' should really be a first class
  // feature so that the user can control where aux info displays go and modes &c don't have to
  // maintain separate special configs for it)

  protected def onElemAt (loc :Loc)(fn :(Element, Loc, Def) => Unit) {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => index.getOption.flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => abort("No element could be found at the point.")
      case Some((elem, loc)) => codex.resolve(window, project, elem.ref) match {
        case None => abort(s"Unable to resolve referent for $elem")
        case Some(df) => fn(elem, loc, df)
      }
    }
  }

  protected def onEncloser (loc :Loc)(fn :JConsumer[Def]) :Unit =
    reqIndex.encloser(buffer.offset(loc)) match {
      case None => abort("Could not find enclosing type.")
      case Some(df) => fn.accept(df)
    }

  /** Returns the "word" at the specified location in the buffer. */
  protected def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString

  protected def history (kind :Kind) = kind match {
    case Kind.MODULE => wspace.historyRing("codex-module")
    case Kind.TYPE   => wspace.historyRing("codex-type")
    case Kind.FUNC   => wspace.historyRing("codex-func")
    case Kind.VALUE  => wspace.historyRing("codex-value")
    case _           => wspace.historyRing("codex-other")
  }
}
