//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import java.nio.file.Path
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.major.ReadingMode
import scaled.util.{BufferBuilder, Errors}

/** A minor mode which provides fns for interacting with a project's Codex.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
// @Minor(name="codex", tags=Array("project"), stateTypes=Array(classOf[Codex]),
//        desc="""A minor mode that provides project-codex fns.""")
class CodexMode (env :Env, major :ReadingMode) extends CodexMinorMode(env) {

  /** Used when highlighting uses in our buffer. */
  val highlights = Value(Seq[Use]())
  highlights.onChange { (nuses, ouses) =>
    ouses foreach upHighlight(false)
    nuses foreach upHighlight(true)
    window.visits() = new Visit.List("occurrence", nuses.toSeq.sortBy(_.offset).map(
      u => Visit(buffer.store, u.offset)))
  }
  private def upHighlight (on :Boolean)(use :Use) :Unit = {
    val start = buffer.loc(use.offset) ; val end = buffer.loc(use.offset+use.length)
    if (on) buffer.addTag(EditorConfig.matchStyle, start, end)
    else buffer.removeTag(EditorConfig.matchStyle, start, end)
  }

  // request that our store be indexed (which should eventually populate `index`)
  {
    def bstore = buffer.storeV()
    // don't attempt to index non- or not-yet-existent files
    def reindex () = if (bstore.exists) codex.queueReindex(project, bstore, false)
    if (!project.compiler.recompileOnSave) note(buffer.storeV.onValueNotify { store => reindex })
    else note(project.compiler.compiled.onValue { path => if (Some(path) == bstore.file) reindex })
  }

  override def keymap = super.keymap.
    bind("describe-codex", "C-h c").

    bind("codex-visit-module", "C-c C-v C-m").
    bind("codex-visit-type",   "C-c C-v C-t").
    bind("codex-visit-func",   "C-c C-v C-f").
    bind("codex-visit-value",  "C-c C-v C-v").
    bind("codex-visit-super",  "C-c C-v C-s").

    bind("codex-summarize-module",   "C-c C-s C-m").
    bind("codex-summarize-type",     "C-c C-s C-t").
    bind("codex-summarize-encloser", "C-c C-z").

    bind("codex-summarize-type",    "C-c C-j").
    // bind("codex-visit-type-member", "C-c C-k").

    // bind("codex-describe-element",  "C-c C-d").
    bind("codex-summarize-element", "S-C-c S-C-d").
    bind("codex-debug-element",     "C-c S-C-d").

    bind("codex-find-uses",         "C-c C-f").
    // bind("codex-visit-element",     "M-.").
    bind("codex-highlight-element", "C-c C-h");

  override def deactivate () :Unit = {
    super.deactivate()
    highlights() = Seq()
  }

  //
  // FNs

  @Fn("Describes the internals of the Codex indices.")
  def describeCodex () :Unit = {
    val bb = new BufferBuilder(view.width()-1)
    codex.describeSelf(bb)
    window.focus.visit(bb.applyTo(project.createBuffer(s"*codex*", "help")))
  }

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
    if (rels.isEmpty) abort(s"No $rel found for '${df.name}'.")
    val ref = rels.iterator.next
    codex.resolve(project, ref) match {
      case None     => abort("Unable to resolve: $ref")
      case Some(df) => visit(df)
    }
  }

  @Fn("""Queries for a module (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeModule () :Unit = codexSummarize("Module:", Kind.MODULE);

  @Fn("""Queries for a type (completed by the project's Codex) and displays its summary.""")
  def codexSummarizeType () :Unit = codexSummarize("Type:", Kind.TYPE);

  @Fn("""Queries for a type (completed by the project's Codex) then queries for a
         member of that type and visits it.""")
  def codexVisitTypeMember () :Unit = codexRead("Type:", Kind.TYPE) { df =>
    val mems = df.members.toSeq
    if (mems.isEmpty) visit(df) // if the def has no members, just jump to it
    else {
      val comp = new Completer[Def]() {
        def complete (glob :String) = Future.success(Completion(glob, mems, true)(_.globalRef.id))
        // take them to the type if they don't make any attempt to select a member
        override def commit (comp :Completion[Def], curval :String) =
          if (curval == "") Some(df) else super.commit(comp, curval)
      }
      window.mini.read("Member:", "", _memHistory.getOrElseUpdate(df, new Ring(8)), comp).
        onSuccess(visit)
    }
  }
  private val _memHistory = MMap[Def,Ring]()

  @Fn("Displays a summary of the type or module that encloses the point. 'Zooming out.'")
  def codexSummarizeEncloser () :Unit = onEncloser(view.point()) { df =>
    def loop (df :Def) :Unit =
      if (df == null) window.popStatus("Could not find enclosing type.")
      else if (Enclosers(df.kind)) summarize(df)
      else loop(df.outer)
    loop(df)
  }
  private val Enclosers = Set(Kind.TYPE, Kind.MODULE)

  @Fn("""Displays the documentation and signature for the element at the point, if it is known to
         the project's Codex.""")
  def codexDescribeElement () :Unit = {
    onElemAt(view.point()) { (elem, loc, df) =>
      view.popup() = codex.mkDefPopup(view, codex.stores(project), df, loc)
    }
  }

  @Fn("""Displays the documentation and signature for the element at the point in a separate buffer
         (rather than in a popup). This can be useful when the docs are very long, or you wish
         to search them, etc.""")
  def codexSummarizeElement () :Unit = {
    onElemAt(view.point()) { (elem, loc, df) =>
      val info = codex.summarizeDef(view, codex.stores(project), df)
      val name = s"${df.name}:${df.qualifier}"
      val buf = project.createBuffer(name, project.codexBufferState("codex-info"))
      buf.delete(buf.start, buf.end)
      buf.append(info.lines)
      frame.visit(buf)
    }
  }

  @Fn("Displays debugging info for all elements on the current line.")
  def codexShowLineElements () :Unit = {
    val loc = view.point()
    val elems = reqIndex.elements(loc.row).toSeq
    if (elems.isEmpty) abort("No Codex elements on current line.")
    view.popup() = Popup.text(elems map(_.toString), Popup.UpRight(loc))
    elems foreach println
    highlights() = elems collect {
      case use :Use => use
      case df  :Def => new Use(df.ref, df.kind, df.offset, df.name.length)
    }
  }

  @Fn("Displays debugging info for the Codex element at the point.")
  def codexDebugElement () :Unit = onElemAt(view.point()) {
    (elem, loc, df) => view.popup() = codex.mkDebugPopup(df, loc)
  }

  @Fn("Displays debugging info for the Codex element enclosing the point.")
  def codexDebugEncloser () :Unit = onEncloser(view.point()) {
    df => view.popup() = codex.mkDebugPopup(df, buffer.loc(df.offset))
  }

  @Fn("Highlights all occurrences of an element in the current buffer.")
  def codexHighlightElement () :Unit = {
    onElemAt(view.point()) { (elem, loc, df) =>
      val bufSource = Codex.toSource(buffer.store)
      val dfRef = df.ref
      val usesMap = codex.store(project).usesOf(df).toMapV // source -> uses
      def mkUse (offset :Int) = new Use(dfRef, df.kind, offset, df.name.length)

      // create a set of all uses in this buffer, for highlighting
      val localUses = Seq.builder[Use]()
      usesMap.get(bufSource) foreach { offsets => localUses ++= offsets map mkUse }
      if (df.source() == bufSource) localUses += mkUse(df.offset)
      highlights() = localUses.build()

      // report the number of uses found in this buffer, and elsewhere in the project
      val count = usesMap.map((src, us) => if (src != bufSource) us.length else 0).fold(0)(_ + _)
      window.emitStatus(s"${highlights().size} occurrences in this buffer, $count in other files.")
    }
  }

  @Fn("Displays all uses of the element at the point in a separate buffer.")
  def codexFindUses () :Unit = {
    onElemAt(view.point()) { (elem, loc, df) =>
      val initState = project.codexBufferState("codex-find-uses", df)
      window.focus.visit(project.createBuffer(s"*codex: ${df.name}*", initState))
    }
  }

  @Fn("""Navigates to the referent of the elmeent at the point, if it is known to this project's
         Codex.""")
  def codexVisitElement () :Unit = {
    onElemAt(view.point())((_, _, df) => visit(df))
  }

  @Fn("Initiates a reindexing of the current project.")
  def codexReindexProject () :Unit = {
    codex.queueReindexAll(project)
  }

  @Fn("Initiates a reindexing of file in the current buffer.")
  def codexReindexBuffer () :Unit = {
    codex.queueReindex(project, buffer.store, true)
  }

  @Fn("""Initiates a debug reindexing of file in the current buffer. The results will be dumped
         to stdout instead of used to populate the index.""")
  def codexDebugReindexBuffer () :Unit = {
    codex.debugReindex(project, buffer.store)
  }

  private def bufferFile :Path = buffer.store.file getOrElse { abort(
      "This buffer has no associated file. A file is needed to detect tests.") }

  // TODO: codexReindexWorkspace?
}
