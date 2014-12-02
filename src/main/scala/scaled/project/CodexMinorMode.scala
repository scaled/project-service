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

  // TODO: it's possible that our buffer's file could change and become part of a new project;
  // do we really want to handle that crazy case?
  val project = Project(buffer)
  val codex = project.pspace.codex

  /** The most recent index for the buffer's source file, if any. */
  val index = OptValue[SourceIndex]()
  // if our store gets indexed, store it in `index`
  note(project.indexer.indexed.onValue { idx => if (idx.store == buffer.store) index() = idx })

  protected def codexRead (prompt :String, kind :Kind)(fn :Def => Unit) {
    window.mini.read(prompt, wordAt(view.point()), codex.history(kind),
                     codex.completer(project, kind)).onSuccess(fn)
  }

  protected def codexVisit (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => codex.visit(window, view, df))

  protected def codexSummarize (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => codex.summarize(window, view, df))

  protected def onElemAt (loc :Loc)(fn :(Element, Loc, Def) => Unit) :Unit = {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => index.getOption.flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => window.popStatus("No element could be found at the point.")
      case Some((elem, loc)) => codex.resolve(project, elem.ref) match {
        case None => window.popStatus(s"Unable to resolve referent for $elem")
        case Some(df) => fn(elem, loc, df)
      }
    }
  }

  protected def onEncloser (loc :Loc)(fn :(Def => Unit)) :Unit = index.getOption match {
    case None => window.popStatus("No Codex index available for this file.")
    case Some(idx) => idx.encloser(buffer.offset(loc)) match {
      case None => window.popStatus("Could not find enclosing type.")
      case Some(df) => fn(df)
    }
  }

  /** Returns the "word" at the specified location in the buffer. */
  protected def wordAt (loc :Loc) :String = {
    import Chars._
    val p = view.point()
    val pstart = buffer.scanBackward(isNotWord, p)
    val start = if (isWord(buffer.charAt(pstart))) pstart else buffer.forward(pstart, 1)
    val end = if (!isWord(buffer.charAt(start))) start
              else buffer.scanForward(isNotWord, p)
    buffer.region(start, end).map(_.asString).mkString
  }
}
