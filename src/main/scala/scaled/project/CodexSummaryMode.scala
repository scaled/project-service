//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import codex.store.ProjectStore
import java.util.Optional
import scala.collection.mutable.ArrayBuffer
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
    import scala.collection.convert.WrapAsScala._
    var start = 0
    sig.text.split(System.lineSeparator) map { l =>
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
  }
  case class DefMembers (df :Def) extends Target {
    def name = s"${df.name}:${df.qualifier}"
  }
  case class TopLevelMembers (store :ProjectStore) extends Target {
    def name = s"${store.name} defs"
  }

  def visitDef (editor :Editor, df :Def) {
    visit(editor, DefMembers(df))
  }

  def visitTopLevel (editor :Editor, store :ProjectStore) {
    visit(editor, TopLevelMembers(store))
  }

  private def visit (editor :Editor, tgt :Target) {
    val view = editor.bufferConfig(tgt.name).reuse().mode("codex-summary", tgt).create()
    editor.visitBuffer(view.buffer)
  }
}

@Major(name="codex-summary", tags=Array("project"),
       desc="""A major mode that displays a summary of a def and its members.""")
class CodexSummaryMode (env :Env, tgt :CodexSummaryMode.Target) extends ReadingMode(env) {
  import scala.collection.convert.WrapAsScala._
  import CodexSummaryMode._

  val pspace = ProjectSpace(env)
  val project = (tgt match {
    case TopLevelMembers(store) => store
    case DefMembers(df) => df.project
  }).asInstanceOf[Project#CodexStore].owner
  // resolve our project and store it in the buffer for our project and codex minor modes
  buffer.state[Project].update(project)

  override def keymap = super.keymap.
    bind("o",     "zoom-out").
    bind("<",     "zoom-out").
    bind("i",     "zoom-in").
    bind(">",     "zoom-in").
    bind("SPACE", "toggle-docs").
    bind("v",     "visit").
    bind(".",     "visit").
    bind("ENTER", "visit-or-zoom");

  //
  // FNs

  @Fn("Displays a summary of the def that encloses the def summarized in this buffer.")
  def zoomOut () :Unit = tgt match {
    case TopLevelMembers(_) => project.visitDescription(editor)
    case DefMembers(df) => df.outer match {
      case null => visitTopLevel(editor, df.project)
      case odef => visitDef(editor, odef)
    }
  }

  @Fn("Displays a summary of the member def at the point.")
  def zoomIn () :Unit = act(view.point(), (i,_) => i.zoomIn())

  @Fn("Visit the def at the point.")
  def visit () :Unit = act(view.point(), (i,_) => i.visit())

  @Fn("Zooms in on modules and types and visits funcs and values.")
  def visitOrZoom () :Unit = act(view.point(), (i,_) => i.visitOrZoom())

  @Fn("Expands or contracts the documentation of the def at the point.")
  def toggleDocs () :Unit = act(view.point(), _.toggle(_))

  //
  // Implementation details

  val infs = ArrayBuffer[Info]() ; {
    val psvc = env.msvc.service[ProjectService]
    val docr = new DocReader()
    def add (defs :Seq[Def]) {
      // group defs by access, then within access sort them by flavor, then name
      val byAcc = defs.groupBy(_.access)
      for (acc <- Access.values) {
        byAcc.get(acc) foreach { defs =>
          if (acc != Access.PUBLIC) infs += new AccessInfo(acc)
          for (mem <- defs.sortBy(d => (d.flavor, d.name))) {
            val docf = psvc.docFormatter(mem.source.fileExt)
            /*if (mem.exported)*/ infs += new DefInfo(mem, docf, docr, "  ")
          }
        }
      }
    }
    tgt match {
      case TopLevelMembers(store) => // otherwise show the top-level members
        infs += new ProjectInfo(store)
        add(store.topLevelDefs.toSeq)
      case DefMembers(df) => // if we have a def, show it and its members
        def addParent (df :Def) :Unit = if (df != null) {
          addParent(df.outer)
          infs += new DefInfo(df, psvc.docFormatter(df.source.fileExt), docr, "")
        }
        infs += new ProjectInfo(df.project)
        addParent(df)
        add(df.members.toSeq)
    }
    view.point() = Loc.Zero
  }

  def act (p :Loc, fn :(Info, Int) => Unit) {
    def loop (line :Int, ii :Int, off :Int) :Unit =
      if (ii == infs.length) fn(infs(ii-1), off)
      else {
        val inf = infs(ii)
        if (line < inf.length) fn(inf, off)
        else loop(line-inf.length, ii+1, off+inf.length)
      }
    loop(p.row, 0, 0)
  }

  trait Info {
    def zoomIn () :Unit
    def visit () :Unit
    def visitOrZoom () :Unit
    def toggle (off :Int) :Unit
    def length :Int
  }

  class ProjectInfo (store :ProjectStore) extends Info {
    val title = s"project ${store.name}"
    buffer.append(Seq(Line(title), Line("-" * title.length)))
    buffer.split(buffer.end)
    buffer.split(buffer.end)

    def zoomIn () {} // TODO?
    def visit () {} // TODO?
    def visitOrZoom () {} // TODO?
    def toggle (off :Int) {} // TODO?
    def length = 3
  }

  class AccessInfo (acc :Access) extends Info {
    // if we're not public, append a string representation to the buffer
    buffer.append(Seq(Line(s" ${acc.toString.toLowerCase}:")))
    buffer.split(buffer.end)

    def zoomIn () {} // noop
    def visit () {} // noop
    def visitOrZoom () {} // noop
    def toggle (off :Int) {} // noop
    def length = 1
  }

  class DefInfo (val df :Def, docf :DocFormatterPlugin, docr :DocReader, indent :String)
      extends Info {
    val source = df.source

    val doc = df.doc
    val fmt = if (doc.isPresent) docf.format(df, doc.get, docr.resolve(source, doc.get))
              else DocFormatterPlugin.NoDoc
    val summary :Seq[LineV] = fmt.summary(indent, view.width()-1)
    lazy val full :Seq[LineV] = fmt.full(indent, view.width()-1)
    var docExpanded = false

    val sig :Seq[LineV] = df.sig match {
      case sigO if (!sigO.isPresent) => Seq(Line(s"$indent<no sig: $df>"))
      case sigO => formatSig(sigO.get, indent)
    }

    def length :Int = (if (docExpanded) full else summary).length + sig.length

    def zoomIn () = pspace.codex.summarize(editor, view, df)
    def visit () = pspace.codex.visit(editor, view, df)
    def visitOrZoom () = df.kind match {
      case Kind.MODULE | Kind.TYPE if (Some(df) != tgt) => zoomIn()
      case _ => visit()
    }

    def toggle (offset :Int) :Unit = if (full.length > 0) {
      val (rows, nlines) = if (docExpanded) (full.length, summary) else (summary.length, full)
      val start = Loc(offset, 0)
      buffer.replace(start, start + (rows, 0), nlines :+ Line.Empty)
      docExpanded = !docExpanded
    }

    // start by appending our summary and signature to the buffer
    buffer.append(summary ++ sig)
    buffer.split(buffer.end)

    private def stripDoc (line :String) = {
      // strip leading whitespace
      val l1 = line.replaceAll("^\\s*", "")
      // if this is Java or Scala, strip Javadoc cruft (TODO: how will we generalize this to more
      // languages? Have the Codex provide a doc post-processor?)
      val l2 = source.fileExt match {
        case "java"|"scala" =>
          l1.replaceAll("^/\\*\\s*", "").replaceAll("\\s*\\*/\\s*$", "").replaceAll("^\\*\\s*", "")
        case _ => l1
      }
      if (l2.length == 0) None else Some(l2)
    }

    private def toDocLine (line :String) =
      Line.builder(indent + line).withStyle(CodeConfig.docStyle).build()
  }
}
