//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import codex.store.ProjectStore
import java.util.Optional
import scaled._
import scaled.code.CodeConfig
import scaled.major.ReadingMode
import scaled.util.BufferBuilder

object CodexSummaryMode {

  def styleFor (kind :Kind) = kind match {
    case Kind.MODULE => Some(CodeConfig.moduleStyle)
    case Kind.TYPE   => Some(CodeConfig.typeStyle)
    case Kind.FUNC   => Some(CodeConfig.functionStyle)
    case Kind.VALUE  => Some(CodeConfig.variableStyle)
    case _           => None
  }

  def formatSig (sig :Sig, indent :String) :Seq[LineV] = {
    var start = 0
    sig.text.split(System.lineSeparator).mkSeq map { l =>
      val len = l.length
      val lb = Line.builder(indent + l)
      for (el <- sig.defs ++ sig.uses) {
        val off = el.offset - start
        if (off >= 0 && off < len) styleFor(el.kind) foreach {
          val start = indent.length+off ; val end = start+el.length
          s => lb.withStyle(s, start, end).withTag(el, start, end)
        }
      }
      start += len + System.lineSeparator.length
      lb.build()
    }
  }

  sealed trait Target {
    def name :String
    def project :Project
  }
  case class DefMembers (df :Def) extends Target {
    def name = s"${df.name}:${df.qualifier}"
    def project = df.project.asInstanceOf[Project#CodexStore].owner
  }
  case class TopLevelMembers (store :ProjectStore) extends Target {
    def name = s"${store.name} defs"
    def project = store.asInstanceOf[Project#CodexStore].owner
  }

  def visitDef (win :Window, df :Def) = visit(win, DefMembers(df))
  def visitTopLevel (win :Window, store :ProjectStore) = visit(win, TopLevelMembers(store))

  private def visit (win :Window, tgt :Target) {
    val buf = win.workspace.createBuffer(
      tgt.name, tgt.project.bufferState("codex-summary", tgt), true)
    win.focus.visit(buf)
  }
}

@Major(name="codex-summary", tags=Array("project"),
       desc="""A major mode that displays a summary of a def and its members.""")
class CodexSummaryMode (env :Env, tgt :CodexSummaryMode.Target) extends ReadingMode(env) {
  import CodexSummaryMode._

  val project = Project(buffer)
  import project.pspace

  override def keymap = super.keymap.
    bind("zoom-out",      "o", "<").
    bind("zoom-in",       "i", ">").
    bind("toggle-docs",   "SPACE").
    bind("visit",         "v", ".").
    bind("visit-or-zoom", "ENTER");

  // we use the code mode styles even though we're not a code mode
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets

  //
  // FNs

  @Fn("Displays a summary of the def that encloses the def summarized in this buffer.")
  def zoomOut () :Unit = tgt match {
    case TopLevelMembers(_) => project.visitDescription(window)
    case DefMembers(df) => df.outer match {
      case null => visitTopLevel(window, df.project)
      case odef => visitDef(window, odef)
    }
  }

  @Fn("Displays a summary of the member def at the point.")
  def zoomIn () :Unit = act(view.point(), _.zoomIn())

  @Fn("Visit the def at the point.")
  def visit () :Unit = act(view.point(), _.visit())

  @Fn("Zooms in on modules and types and visits funcs and values.")
  def visitOrZoom () :Unit = act(view.point(), _.visitOrZoom())

  @Fn("Expands or contracts the documentation of the def at the point.")
  def toggleDocs () :Unit = act(view.point(), _.toggle())

  //
  // Implementation details

  class Info extends Line.Tag {
    def zoomIn () {}
    def visit () {}
    def visitOrZoom () {}
    def toggle () {}
    override def key :Any = classOf[Info]
  }
  private val NoInfo = new Info()

  private def act (p :Loc, fn :Info => Unit) = fn(buffer.line(p).lineTag(NoInfo))

  if (buffer.start == buffer.end) {
    val psvc = env.msvc.service[ProjectService]
    val docr = new DocReader()
    def add (defs :Seq[Def]) {
      // group defs by access, then within access sort them by flavor, then name
      val byAcc = defs.groupBy(_.access)
      var wroteAcc :Access = null
      for (acc <- Access.values) {
        byAcc.get(acc) foreach { defs =>
          for (mem <- defs.sortBy(d => (d.flavor, d.name))) {
            val docf = psvc.docFormatter(mem.source.fileExt)
            if (mem.kind != Kind.SYNTHETIC) {
              // defer the writing of the access separator until we *actually* write a def with
              // this access level; otherwise we could write the level and then discover that all
              // the defs in it are invisible (synthetic)
              if (acc != Access.PUBLIC && acc != wroteAcc) {
                buffer.append(Seq(Line(s" ${acc.toString.toLowerCase}:")))
                buffer.split(buffer.end)
                wroteAcc = acc
              }
              addDefInfo(mem, docf, docr, "  ")
            }
          }
        }
      }
    }
    tgt match {
      case TopLevelMembers(store) => // otherwise show the top-level members
        addProjectInfo(store)
        add(store.topLevelDefs.toSeq)
      case DefMembers(df) => // if we have a def, show it and its members
        def addParent (df :Def) :Unit = if (df != null) {
          addParent(df.outer)
          addDefInfo(df, psvc.docFormatter(df.source.fileExt), docr, "")
          // tack an extra blank line after the module parent
          if (df.kind == Kind.MODULE) buffer.split(buffer.end)
        }
        addProjectInfo(df.project)
        addParent(df)
        add(df.members.toSeq)
    }
    view.point() = Loc.Zero
  }

  private def addProjectInfo (store :ProjectStore) {
    val title = s"project ${store.name}"
    buffer.append(Seq(Line(title), Line("-" * title.length)))
    buffer.split(buffer.end)
    buffer.split(buffer.end)
  }

  private def addDefInfo (df :Def, docf :DocFormatterPlugin, docr :DocReader, indent :String) {
    val source = df.source

    val doc = df.doc
    val fmt = if (doc.isPresent) docf.format(df, doc.get, docr.resolve(source, doc.get))
              else DocFormatterPlugin.NoDoc
    val summary :SeqV[LineV] = fmt.summary(indent, view.width()-1)

    val info = new Info() {
      lazy val full :SeqV[LineV] = fmt.full(indent, view.width()-1)
      var docExpanded = false

      override def zoomIn () = pspace.codex.summarize(window, view, df)
      override def visit () = pspace.codex.visit(window, view, df)
      override def visitOrZoom () = df.kind match {
        case Kind.MODULE | Kind.TYPE if (Some(df) != tgt) => zoomIn()
        case _ => visit()
      }

      override def toggle () = if (full.length > 0) {
        var row = 0 ; while (buffer.line(row).lineTag(NoInfo) != this) row += 1
        val (rows, nlines) = if (docExpanded) (full.length, summary) else (summary.length, full)
        val start = Loc(row, 0)
        val end = buffer.replace(start, start + (rows, 0), nlines :+ Line.Empty)
        var loc = start ; while (loc < end) { buffer.setLineTag(loc, this) ; loc = loc.nextL }
        docExpanded = !docExpanded
      }
    }

    val sig :Seq[LineV] = df.sig match {
      case sigO if (!sigO.isPresent) => Seq(Line(s"$indent<no sig: $df>"))
      case sigO => formatSig(sigO.get, indent)
    }

    // start by appending our summary and signature to the buffer
    var loc = buffer.end
    buffer.append(summary ++ sig)
    // tag all generated lines with our info
    while (loc < buffer.end) { buffer.setLineTag(loc, info) ; loc = loc.nextL }
    buffer.split(buffer.end)
  }
}
