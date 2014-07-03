//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.Codex
import codex.model._
import javafx.scene.control.Tooltip
import reactual.{Value, OptValue}
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.ReadingMode
import scaled.util.Chars

/** A minor mode which provides fns for interacting with a project's Codex.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="codex",
       tags=Array("project"),
       desc="""A minor mode that provides project-codex fns.""")
class CodexMode (env :Env, psvc :ProjectService, major :ReadingMode) extends MinorMode(env) {

  val project :Project = (major match {
    case pmode :HasProjectMode => pmode.project
    case _                     => psvc.projectFor(buffer.store)
  }).reference(this)

  /** The most recent index for the buffer's source file, if any. */
  val index = OptValue[SourceIndex]()
  // if our store gets indexed, store it in `index`
  note(project.codex.indexed.onValue { idx => if (idx.store == buffer.store) index() = idx })
  // request that our store be indexed (which should eventually populate `index`)
  note(buffer.storeV.onValueNotify { store =>
    // don't attempt to index non- or not-yet-existent files
    if (buffer.store.exists) project.codex.reindex(store)
  })

  override def keymap = Seq(
    // "C-h c"   -> "describe-codex", // TODO:?

    "C-c C-v C-m" -> "codex-visit-module",
    "C-c C-v C-t" -> "codex-visit-type",
    "C-c C-v C-f" -> "codex-visit-func",
    "C-c C-v C-v" -> "codex-visit-value",

    "C-c C-s C-m" -> "codex-summarize-module",
    "C-c C-s C-t" -> "codex-summarize-type",
    "C-c C-z"     -> "codex-summarize-encloser",

    // TEMP: implement local key bindings
    "C-c C-i"     -> "codex-import-type",
    "C-c C-j"     -> "codex-summarize-type",
    "C-c C-k"     -> "codex-visit-type",

    "C-c C-d"     -> "codex-describe-element",
    "C-c S-C-d"   -> "codex-debug-element",
    "M-."         -> "codex-visit-element",
    "M-,"         -> "codex-visit-pop"
  )

  override def dispose () {
    super.dispose()
    project.release(this)
  }

  //
  // FNs

  @Fn("Queries for a module (completed by the project's Codex) and navigates to its definition.")
  def codexVisitModule () :Unit = codexVisit("Module:", Kind.MODULE)

  @Fn("Queries for a type (completed by the project's Codex) and navigates to its definition.")
  def codexVisitType () :Unit = codexVisit("Type:", Kind.TYPE)

  @Fn("Queries for a function (completed by the project's Codex) and navigates to its definition.")
  def codexVisitFunc () :Unit = codexVisit("Function:", Kind.FUNC)

  @Fn("Queries for a value (completed by the project's Codex) and navigates to its definition.")
  def codexVisitValue () :Unit = codexVisit("Value:", Kind.VALUE)

  @Fn("""Queries for a module (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeModule () :Unit = codexSummarize("Module:", Kind.MODULE);

  @Fn("""Queries for a type (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeType () :Unit = codexSummarize("Type:", Kind.TYPE);

  @Fn("Displays a summary of the type or module that encloses the point. 'Zooming out.'")
  def codexSummarizeEncloser () :Unit = index.getOption match {
    case None => editor.popStatus("No Codex index available for this file.")
    case Some(idx) => idx.encloser(buffer.offset(view.point())) match {
      case None => editor.popStatus("Could not find enclosing type.")
      case Some(df) =>
        def loop (df :Def) :Unit =
          if (df == null) editor.popStatus("Could not find enclosing type.")
          else if (Enclosers(df.kind)) project.codex.summarize(editor, view, df)
          else loop(df.outer)
        loop(df)
    }
  }
  private val Enclosers = Set(Kind.TYPE, Kind.MODULE)

  @Fn("""Displays the documentation and signature for the element at the point, if it is known to
         the project's Codex.""")
  def codexDescribeElement () {
    onElemAt(view.point(), (elem, loc, df) => view.popup() = mkDefPopup(elem, loc, df))
  }

  @Fn("Displays debugging info for the Codex element at the point.")
  def codexDebugElement () {
    onElemAt(view.point(), (elem, loc, df) => view.popup() = mkDebugPopup(elem, loc, df))
  }

  @Fn("""Navigates to the referent of the elmeent at the point, if it is known to this project's
         Codex.""")
  def codexVisitElement () {
    onElemAt(view.point(), (_, _, df) => project.codex.visit(editor, view, df))
  }

  @Fn("Pops to the last place `codex-visit-foo` was invoked.")
  def codexVisitPop () {
    project.codex.visitStack.pop(editor)
  }

  @Fn("Queries for a type (completed by the project's Codex) and adds an import for it.")
  def codexImportType () {
    codexRead("Type:", Kind.TYPE)(insertImport)
  }

  //
  // Implementation details

  private def codexRead (prompt :String, kind :Kind)(fn :Def => Unit) {
    editor.mini.read(prompt, wordAt(view.point()), project.codexHistory(kind),
                     project.codex.completer(kind)).onSuccess(fn)
  }

  private def codexVisit (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => project.codex.visit(editor, view, df))

  private def codexSummarize (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => project.codex.summarize(editor, view, df))

  private def onElemAt (loc :Loc, fn :(Element, Loc, Def) => Unit) :Unit = {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => index.getOption.flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => editor.popStatus("No element could be found at the point.")
      case Some((elem, loc)) =>
        val dopt = project.codex.resolve(elem.ref)
        if (!dopt.isPresent) editor.popStatus(s"Unable to resolve referent for ${elem.ref}")
        else fn(elem, loc, dopt.get)
    }
  }

  // TODO: this should be in java-mode and/or scala-mode...
  private val importM = Matcher.regexp("^import ")
  private val packageM = Matcher.regexp("^package ")
  private val firstDefM = Matcher.regexp("(class|interface|object|trait)")
  private def insertImport (df :Def) {
    val suff = if (major.name == "scala") "" else ";"
    val fqName = df.fqName
    val text = s"import $fqName$suff"

    // first figure out where we're going to stop looking
    val firstDef = buffer.findForward(firstDefM, buffer.start) match {
      case Loc.None => buffer.end
      case loc => loc
    }

    // TODO: handle fancy scala grouped imports...

    // look for an existing "import " statement in the buffer and scan down from there to find the
    // position at which to insert the new statement
    def loop (prev :Loc) :Loc = {
      val next = buffer.findForward(importM, prev.nextStart, firstDef)
      // if we see no more import statements...
      if (next == Loc.None) {
        // if we saw at least one import statement, then insert after the last one we saw
        if (prev != buffer.start) prev.nextStart
        // otherwise fail the search and fall back to inserting after 'package'
        else Loc.None
      }
      else {
        val ltext = buffer.line(next).asString
        // if we have this exact import, abort (we'll catch and report this below)
        if (ltext == text) throw new IllegalStateException(s"$fqName already imported.")
        // if our import sorts earlier than this import, insert here
          else if (text < ltext) next
        // otherwise check the next import statement
          else loop(next)
      }
    }
    try {
      val (loc, lines) = loop(buffer.start) match {
        // if we failed to find existing import statements, look for a package statement
        case Loc.None => buffer.findForward(packageM, buffer.start, firstDef) match {
          case Loc.None =>
            // fuck's sake, put the import at the top of the file (with a blank line after)
            (buffer.start, List(text, "", ""))
          case loc =>
            // insert a blank line after 'package' and then our import
            (loc.nextStart, List("", text, ""))
        }
        case loc =>
        // put the import at the specified location
        (loc, List(text, ""))
      }
      buffer.insert(loc, lines map (Line.apply))
    } catch {
      case ie :IllegalStateException => editor.popStatus(ie.getMessage)
    }
  }

  /** Returns the "word" at the specified location in the buffer. */
  private def wordAt (loc :Loc) :String = {
    import Chars._
    val p = view.point()
    val pstart = buffer.scanBackward(isNotWord, p)
    val start = if (isWord(buffer.charAt(pstart))) pstart else buffer.forward(pstart, 1)
    val end = if (!isWord(buffer.charAt(start))) start
              else buffer.scanForward(isNotWord, p)
    buffer.region(start, end).map(_.asString).mkString
  }

  private def mkDefPopup (elem :Element, loc :Loc, df :Def) :Popup = {
    val text = ArrayBuffer[String]()
    df.doc.ifPresent(new java.util.function.Consumer[Doc]() {
      def accept (doc :Doc) :Unit = try {
        val r = df.source().reader()
        val buf = new Array[Char](doc.length)
        r.skip(doc.offset)
        r.read(buf)
        r.close()
        // TODO: trim leading whitespace up to start.col
        text ++= new String(buf).split(System.lineSeparator)
      } catch {
        case e :Exception => text += e.toString
      }
    })
    df.sig.ifPresent(new java.util.function.Consumer[Sig]() {
      def accept (sig :Sig) {
        text ++= sig.text.split(System.lineSeparator)
        // TODO: use defs and uses to style text
      }
    })
    Popup(text, Popup.UpRight(loc))
  }

  private def mkDebugPopup (elem :Element, loc :Loc, df :Def) :Popup = {
    def safeGet (thunk : => Any) = try thunk.toString catch { case t :Throwable => t.toString }
    val text = ArrayBuffer[String]()
    text += s"ID:    ${df.idToString}"
    text += s"Outer: ${df.outerIdToString}"
    text += s"Kind:  ${df.kind}"
    text += s"Exp:   ${df.exported}"
    text += s"Name:  ${df.name}"
    text += s"Off:   ${df.offset}"
    text += s"Src:   ${safeGet(df.source)}"
    text += s"GID:   ${safeGet(df.globalRef)}"
    Popup(text, Popup.UpRight(loc))
  }
}
