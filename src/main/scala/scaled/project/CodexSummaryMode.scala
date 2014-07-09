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

  sealed trait Target {
    def name :String
  }
  case class DefMembers (df :Def) extends Target {
    def name = s"${df.name}:${df.qualifier}"
  }
  case class TopLevelMembers (store :ProjectStore) extends Target {
    def name = s"${store.name} defs"
  }

  def visitDef (editor :Editor, project :Project, df :Def) {
    visit(editor, project, DefMembers(df))
  }

  def visitTopLevel (editor :Editor, project :Project, store :ProjectStore) {
    visit(editor, project, TopLevelMembers(store))
  }

  private def visit (editor :Editor, proj :Project, tgt :Target) {
    val view = editor.createBuffer(tgt.name, true, Some("codex-summary"), proj.asState, tgt)
    editor.visitBuffer(view.buffer)
  }
}

@Major(name="codex-summary",
       tags=Array("project", "codex"),
       desc="""A major mode that displays a summary of a def and its members.""")
class CodexSummaryMode (env :Env, tgt :CodexSummaryMode.Target) extends ReadingMode(env) {
  import scala.collection.convert.WrapAsScala._
  import CodexSummaryMode._

  // reference our target project, and release it when we're disposed
  val project = buffer.state.req(classOf[Project])
  note(project.reference(buffer))

  override def keymap = super.keymap ++ Seq(
    "o"     -> "zoom-out",
    "<"     -> "zoom-out",
    "i"     -> "zoom-in",
    ">"     -> "zoom-in",
    "SPACE" -> "toggle-docs",
    "v"     -> "visit",
    "."     -> "visit",
    "ENTER" -> "visit-or-zoom"
  )

  //
  // FNs

  @Fn("Displays a summary of the def that encloses the def summarized in this buffer.")
  def zoomOut () :Unit = tgt match {
    case TopLevelMembers(store) => project.codex.projectFor(store) match {
      case Some(stp) => stp.visitDescription(editor, view.width())
      case None      => editor.popStatus("Unable to determine project for this Codex.")
    }
    case DefMembers (df) => df.outer match {
      case null => visitTopLevel(editor, project, df.project)
      case odef => visitDef(editor, project, odef)
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
    val docr = new DocReader()
    def add (defs :Seq[Def]) {
      // group defs by access, then within access sort them by flavor, then name
      val byAcc = defs.groupBy(_.access)
      for (acc <- Access.values) {
        byAcc.get(acc) foreach { defs =>
          if (acc != Access.PUBLIC) infs += new AccessInfo(acc)
          for (mem <- defs.sortBy(d => (d.flavor, d.name))) {
            /*if (mem.exported)*/ infs += new DefInfo(mem, docr, "  ")
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
          infs += new DefInfo(df, docr, "")
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

  class DefInfo (val df :Def, docr :DocReader, indent :String) extends Info {
    val source = df.source

    val doc :Seq[LineV] = df.doc match {
      case doc if (!doc.isPresent) => Seq()
      case doc => docr.resolve(source, doc.get).flatMap(stripDoc).map(toDocLine)
    }
    val firstDoc :LineV = doc.headOption getOrElse(toDocLine("Undocumented"))
    var docExpanded = false

    val sig :Seq[LineV] = df.sig match {
      case sigO if (!sigO.isPresent) => Seq(Line(s"<no sig: $df>"))
      case sigO =>
        val sig = sigO.get
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

    def length :Int = (if (docExpanded) doc.length else 1) + sig.length

    def zoomIn () = project.codex.summarize(editor, view, df)
    def visit () = project.codex.visit(editor, view, df)
    def visitOrZoom () = df.kind match {
      case Kind.MODULE | Kind.TYPE if (Some(df) != tgt) => zoomIn()
      case _ => visit()
    }

    def toggle (offset :Int) :Unit = if (doc.length > 0) {
      val (rows, nlines) = if (docExpanded) (doc.length, Seq(firstDoc)) else (1, doc)
      val start = Loc(offset, 0)
      buffer.replace(start, start + (rows, 0), nlines :+ Line.Empty)
      docExpanded = !docExpanded
    }

    // start by appending our first doc line and signature to the buffer
    buffer.append(firstDoc +: sig)
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

  private def styleFor (kind :Kind) = kind match {
    case Kind.MODULE => Some(CodeConfig.moduleStyle)
    case Kind.TYPE   => Some(CodeConfig.typeStyle)
    case Kind.FUNC   => Some(CodeConfig.functionStyle)
    case Kind.VALUE  => Some(CodeConfig.variableStyle)
    case _           => None
  }
}
