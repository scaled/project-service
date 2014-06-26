//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import java.util.Optional
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.code.CodeConfig
import scaled.major.ReadingMode
import scaled.util.BufferBuilder

@Major(name="codex-summary",
       tags=Array("project", "codex"),
       desc="""A major mode that displays a summary of a def and its members.""")
class CodexSummaryMode (env :Env, val project :Project, tgt :Option[Def])
    extends ReadingMode(env) with HasProjectMode {
  import scala.collection.convert.WrapAsScala._

  // reference our target project, and release it when we're disposed
  project.reference(buffer)
  override def dispose () {
    super.dispose()
    project.release(buffer)
  }

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
    case None => project.visitDescription(editor, view.width())
    case Some(df) => df.outer match {
      case null =>
        val view = editor.createBuffer(s"${project.name} defs", true,
                                       ModeInfo("codex-summary", List(None, project)))
        editor.visitBuffer(view.buffer)
      case odef => project.codex.summarize(editor, view, odef)
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
    infs += new ProjectInfo()
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
      case Some(df) => // if we have a def, show it and its members
        def addParent (df :Def) :Unit = if (df != null) {
          addParent(df.outer)
          infs += new DefInfo(df, docr, "")
        }
        addParent(df)
        add(df.members.toSeq)
      case None => // otherwise show the top-level members
        add(project.codex.projectStore.topLevelDefs.toSeq)
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

  class ProjectInfo extends Info {
    // append our project summary and name
    buffer.append(Seq(Line(s"project ${project.name}")))
    buffer.split(buffer.end)

    def zoomIn () {} // TODO?
    def visit () {} // TODO?
    def visitOrZoom () {} // TODO?
    def toggle (off :Int) {} // TODO?
    def length = 1
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
            if (off >= 0 && off < len) {
              lb.withStyles(stylesFor(el.kind), indent.length+off, indent.length+off+el.length)
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
      Line.builder(indent + line).withStyles(Styles(CodeConfig.docStyle)).build()
  }

  private def stylesFor (kind :Kind) = kind match {
    case Kind.MODULE => Styles(CodeConfig.moduleStyle)
    case Kind.TYPE   => Styles(CodeConfig.typeStyle)
    case Kind.FUNC   => Styles(CodeConfig.functionStyle)
    case Kind.VALUE  => Styles(CodeConfig.variableStyle)
    case _           => Styles.None
  }
}
