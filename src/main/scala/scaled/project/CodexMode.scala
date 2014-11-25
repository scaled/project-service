//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import javafx.scene.control.Tooltip
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.ReadingMode
import scaled.util.{BufferBuilder, Chars, Errors}

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
    if (buffer.store.exists) project.indexer.queueReindex(store, false)
  })

  /** Used when highlighting uses in our buffer. */
  val highlights = Value(Set[Use]())
  highlights.onChange { (nuses, ouses) =>
    ouses foreach upHighlight(false)
    nuses foreach upHighlight(true)
    window.visits() = new Visit.List("occurrence", nuses.toSeq.sortBy(_.offset).map(u => Visit(buffer.store, u.offset)))
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
    bind("codex-visit-super",  "C-c C-v C-s").

    bind("codex-summarize-module",   "C-c C-s C-m").
    bind("codex-summarize-type",     "C-c C-s C-t").
    bind("codex-summarize-encloser", "C-c C-z").

    bind("codex-import-type",    "C-c C-i"). // TODO: this should be in Java/Scala mode
    bind("codex-summarize-type", "C-c C-j").
    bind("codex-visit-type",     "C-c C-k").

    bind("codex-describe-element",  "C-c C-d").
    bind("codex-debug-element",     "C-c S-C-d").
    bind("codex-highlight-element", "C-c C-h").
    bind("codex-find-uses",         "C-c C-f").
    bind("codex-rename-element",    "C-c C-r").
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

  @Fn("""If called inside a method, visits the method it immediately overrides, if any.
         If called inside a class but not a method, visits the class's parent.""")
  def codexVisitSuper () :Unit = onEncloser(view.point()) { df =>
    val rel = if (df.kind == Kind.FUNC) Relation.OVERRIDES else Relation.INHERITS
    val rels = df.relations(rel)
    if (rels.isEmpty) window.popStatus(s"No $rel found for '${df.name}'.")
    else {
      val ref = rels.iterator.next
      pspace.codex.resolve(project, ref) match {
        case None => window.popStatus("Unable to resolve: $ref")
        case Some(df) => pspace.codex.visit(window, view, df)
      }
    }
  }

  @Fn("""Queries for a module (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeModule () :Unit = codexSummarize("Module:", Kind.MODULE);

  @Fn("""Queries for a type (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeType () :Unit = codexSummarize("Type:", Kind.TYPE);

  @Fn("Displays a summary of the type or module that encloses the point. 'Zooming out.'")
  def codexSummarizeEncloser () :Unit = onEncloser(view.point()) { df =>
    def loop (df :Def) :Unit =
      if (df == null) window.popStatus("Could not find enclosing type.")
      else if (Enclosers(df.kind)) pspace.codex.summarize(window, view, df)
      else loop(df.outer)
    loop(df)
  }
  private val Enclosers = Set(Kind.TYPE, Kind.MODULE)

  @Fn("""Displays the documentation and signature for the element at the point, if it is known to
         the project's Codex.""")
  def codexDescribeElement () {
    onElemAt(view.point()) { (elem, loc, df) =>
      view.popup() = CodexUtil.mkDefPopup(env, df, loc)
    }
  }

  @Fn("Displays debugging info for the Codex element at the point.")
  def codexDebugElement () {
    onElemAt(view.point())((elem, loc, df) => view.popup() = CodexUtil.mkDebugPopup(df, loc))
  }

  @Fn("Highlights all occurrences of an element in the current buffer.")
  def codexHighlightElement () {
    onElemAt(view.point()) { (elem, loc, df) =>
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
      val count = usesMap.map((src, us) => if (src != bufSource) us.length else 0).fold(0)(_ + _)
      window.emitStatus(s"${highlights().size} occurrences in this buffer, $count in other files.")
    }
  }

  @Fn("Displays all uses of the element at the point in a separate buffer.")
  def codexFindUses () {
    onElemAt(view.point()) { (elem, loc, df) =>
      val state = project.bufferState("codex-find-uses", df)
      window.focus.visit(wspace.createBuffer(s"*codex: ${df.name}*", state))
    }
  }

  private val renameHistory = new Ring(editor.config(EditorConfig.historySize))

  class Renamer (df :Def, src :Source, offsets :Array[Int]) {
    val store = PSpaceCodex.toStore(src)
    lazy val buffer = wspace.openBuffer(store)
    lazy val locs = {
      // convert the offsets to locs and sort them in reverse order
      // (that way when we replace them, the earlier locs don't mess up the later locs)
      val offs = if (src == df.source) df.offset +: offsets else offsets
      (offs map buffer.loc).sortWith(_ > _).toSeq
    }

    def validate (elM :Matcher) = locs foreach { loc =>
      if (!buffer.line(loc).matches(elM, loc.col)) throw Errors.feedback(
        s"$store not in sync with index @ $loc. Can't rename.")
    }

    def replace (nameL :LineV) = locs foreach { loc => buffer.replace(loc, df.name.length, nameL) }
  }

  @Fn("Renames all occurrences of an element.")
  def codexRenameElement () {
    onElemAt(view.point()) { (elem, loc, df) =>
      if (df.kind != Kind.FUNC && df.kind != Kind.VALUE) throw Errors.feedback(
        "Rename only supported for methods and variables. Not types or packages.")

      val renamers = project.store.usesOf(df).toMapV.map(new Renamer(df, _, _))
      if (renamers.isEmpty) throw Errors.feedback(
        "No uses found for element at point. Perhaps try codex-reindex-project?")

      def doit (save :Boolean) {
        val elM = Matcher.exact(df.name)
        renamers.foreach(_.validate(elM))
        window.mini.read("New name:", df.name, renameHistory, Completer.none).onSuccess { nm =>
          if (nm != df.name) {
            val nameL = Line(nm)
            renamers.foreach(_.replace(nameL))
            if (save) renamers.foreach(_.buffer.save())
          }
        }
      }

      // if there are occurrences outside the current buffer, confirm the rename
      if (renamers.size == 1 && renamers(0).store == buffer.store) doit(false)
      else window.mini.readYN(
        s"'${df.name}' occurs in ${renamers.size-1} source file(s) not including this one. " +
          "Undoing the rename will not be trivial, continue?").onSuccess { yes =>
        if (yes) doit(true)
      }
    }
  }

  @Fn("""Navigates to the referent of the elmeent at the point, if it is known to this project's
         Codex.""")
  def codexVisitElement () {
    onElemAt(view.point())((_, _, df) => pspace.codex.visit(window, view, df))
  }

  @Fn("Queries for a type (completed by the project's Codex) and adds an import for it.")
  def codexImportType () {
    codexRead("Type:", Kind.TYPE)(insertImport)
  }

  @Fn("Initiates a reindexing of the current project.")
  def codexReindexProject () {
    project.store.reindex()
  }

  @Fn("Initiates a reindexing of file in the current buffer.")
  def codexReindexBuffer () {
    project.indexer.queueReindex(buffer.store, true)
  }

  @Fn("""Initiates a debug reindexing of file in the current buffer. The results will be dumped
         to stdout instead of used to populate the index.""")
  def codexDebugReindexBuffer () {
    project.indexer.debugReindex(buffer.store)
  }

  // TODO: codexReindexWorkspace?

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

  private def onElemAt (loc :Loc)(fn :(Element, Loc, Def) => Unit) :Unit = {
    val elloc = buffer.tagsAt(classOf[Element], loc) match {
      case el :: _ => Some(el.tag -> loc.atCol(el.start))
      case Nil     => index.getOption.flatMap(_.elementAt(loc) map(
        el => (el, buffer.loc(el.offset))))
    }
    elloc match {
      case None => window.popStatus("No element could be found at the point.")
      case Some((elem, loc)) => pspace.codex.resolve(project, elem.ref) match {
        case None => window.popStatus(s"Unable to resolve referent for $elem")
        case Some(df) => fn(elem, loc, df)
      }
    }
  }

  private def onEncloser (loc :Loc)(fn :(Def => Unit)) :Unit = index.getOption match {
    case None => window.popStatus("No Codex index available for this file.")
    case Some(idx) => idx.encloser(buffer.offset(loc)) match {
      case None => window.popStatus("Could not find enclosing type.")
      case Some(df) => fn(df)
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
}
