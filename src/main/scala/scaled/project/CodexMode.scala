//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import javafx.scene.control.Tooltip
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.ReadingMode
import scaled.util.{BufferBuilder, Chars}

/** A minor mode which provides fns for interacting with a project's Codex.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="codex", tags=Array("project"),
       desc="""A minor mode that provides project-codex fns.""")
class CodexMode (env :Env, major :ReadingMode) extends MinorMode(env) {

  val project = Project(buffer)
  import project.pspace

  /** The most recent index for the buffer's source file, if any. */
  val index = OptValue[SourceIndex]()
  // if our store gets indexed, store it in `index`
  note(project.indexer.indexed.onValue { idx => if (idx.store == buffer.store) index() = idx })
  // request that our store be indexed (which should eventually populate `index`)
  note(buffer.storeV.onValueNotify { store =>
    // don't attempt to index non- or not-yet-existent files
    if (buffer.store.exists) project.indexer.queueReindex(store)
  })

  /** Used when highlighting uses in our buffer. */
  val highlights = Value(Set[Use]())
  highlights.onChange { (nuses, ouses) =>
    ouses foreach upHighlight(false)
    nuses foreach upHighlight(true)
  }
  private def upHighlight (on :Boolean)(use :Use) {
    val start = buffer.loc(use.offset) ; val end = buffer.loc(use.offset+use.length)
    if (on) buffer.addTag(EditorConfig.matchStyle, start, end)
    else buffer.removeTag(EditorConfig.matchStyle, start, end)
  }

  override def keymap = super.keymap.
    // "C-h c"   -> "describe-codex", // TODO:?
    bind("codex-visit-module", "C-c C-v C-m").
    bind("codex-visit-type",   "C-c C-v C-t").
    bind("codex-visit-func",   "C-c C-v C-f").
    bind("codex-visit-value",  "C-c C-v C-v").

    bind("codex-summarize-module",   "C-c C-s C-m").
    bind("codex-summarize-type",     "C-c C-s C-t").
    bind("codex-summarize-encloser", "C-c C-z").

    // TEMP: implement local key bindings
    bind("codex-import-type",    "C-c C-i").
    bind("codex-summarize-type", "C-c C-j").
    bind("codex-visit-type",     "C-c C-k").

    bind("codex-describe-element",  "C-c C-d").
    bind("codex-debug-element",     "C-c S-C-d").
    bind("codex-highlight-element", "C-c C-h").
    bind("codex-visit-element",     "M-.");

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
    case None => window.popStatus("No Codex index available for this file.")
    case Some(idx) => idx.encloser(buffer.offset(view.point())) match {
      case None => window.popStatus("Could not find enclosing type.")
      case Some(df) =>
        def loop (df :Def) :Unit =
          if (df == null) window.popStatus("Could not find enclosing type.")
          else if (Enclosers(df.kind)) pspace.codex.summarize(window, view, df)
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

  @Fn("Highlights all occurances of an element in the current buffer.")
  def codexHighlightElement () {
    onElemAt(view.point(), (elem, loc, df) => {
      val bufSource = PSpaceCodex.toSource(buffer.store)
      val dfRef = df.ref
      val usesMap = project.store.usesOf(df).toMapV // source -> uses
      def mkUse (offset :Int) = new Use(dfRef, df.kind, offset, df.name.length)

      // create a set of all uses in this buffer, for highlighting
      val localUses = Set.builder[Use]()
      usesMap.get(bufSource) foreach { offsets => localUses ++= offsets map mkUse }
      if (df.source() == bufSource) localUses += mkUse(df.offset)
      highlights() = localUses.build()

      // report the number of uses found in this buffer, and elsewhere in the project
      val count = usesMap.map((src, us) => if (src != bufSource) us.length else 0).reduce(_ + _)
      window.emitStatus(s"${highlights().size} occurances in this buffer, $count in other files.")
    })
  }

  @Fn("""Navigates to the referent of the elmeent at the point, if it is known to this project's
         Codex.""")
  def codexVisitElement () {
    onElemAt(view.point(), (_, _, df) => pspace.codex.visit(window, view, df))
  }

  @Fn("Queries for a type (completed by the project's Codex) and adds an import for it.")
  def codexImportType () {
    codexRead("Type:", Kind.TYPE)(insertImport)
  }

  //
  // Implementation details

  private def codexRead (prompt :String, kind :Kind)(fn :Def => Unit) {
    window.mini.read(prompt, wordAt(view.point()), pspace.codex.history(kind),
                     pspace.codex.completer(project, kind)).onSuccess(fn)
  }

  private def codexVisit (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => pspace.codex.visit(window, view, df))

  private def codexSummarize (prompt :String, kind :Kind) :Unit =
    codexRead(prompt, kind)(df => pspace.codex.summarize(window, view, df))

  private def onElemAt (loc :Loc, fn :(Element, Loc, Def) => Unit) :Unit = {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => index.getOption.flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => window.popStatus("No element could be found at the point.")
      case Some((elem, loc)) =>
        val dopt = pspace.codex.resolve(project, elem.ref)
        if (!dopt.isPresent) window.popStatus(s"Unable to resolve referent for ${elem.ref}")
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
      case ie :IllegalStateException => window.popStatus(ie.getMessage)
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
    val bb = new BufferBuilder(view.width()-2)
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
    Popup.lines(bb.lines, Popup.UpRight(loc))
  }

  private def mkDebugPopup (elem :Element, loc :Loc, df :Def) :Popup = {
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
