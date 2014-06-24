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
class CodexSummaryMode (env :Env, val project :Project, df :Def)
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
  def zoomOut () {
    if (df.outerId == null) editor.popStatus("This def is not enclosed by another def.")
    else project.codex.summarize(editor, view, df.project.`def`(df.outerId))
  }

  @Fn("Displays a summary of the member def at the point.")
  def zoomIn () {
    val (inf, _) = find(view.point().row)
    project.codex.summarize(editor, view, inf.df)
  }

  @Fn("Visit the def at the point.")
  def visit () {
    val (inf, _) = find(view.point().row)
    inf.visit()
  }

  @Fn("Zooms in on modules and types and visits funcs and values.")
  def visitOrZoom () {
    val (inf, _) = find(view.point().row)
    inf.df.kind match {
      case Kind.MODULE | Kind.TYPE => project.codex.summarize(editor, view, inf.df)
      case _ => inf.visit()
    }
  }

  @Fn("Expands or contracts the documentation of the def at the point.")
  def toggleDocs () {
    val (inf, off) = find(view.point().row)
    inf.toggle(off)
  }

  //
  // Implementation details

  val infs = ArrayBuffer[DefInfo]() ; {
    val docr = new DocReader()
    infs += new DefInfo(df, docr, "")
    for (mem <- df.members.toSeq.sortBy(d => (d.kind, d.name))) {
      if (mem.exported) infs += new DefInfo(mem, docr, "  ")
    }
    view.point() = Loc.Zero
  }

  def find (line :Int, ii :Int = 0, off :Int = 0) :(DefInfo, Int) = {
    if (ii == infs.length) (infs(ii-1), off)
    else {
      val inf = infs(ii)
      if (line < inf.length) (inf, off)
      else find(line-inf.length, ii+1, off+inf.length)
    }
  }

  class DefInfo (val df :Def, docr :DocReader, indent :String) {
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

    def toggle (offset :Int) :Unit = if (doc.length > 0) {
      val (rows, nlines) = if (docExpanded) (doc.length, Seq(firstDoc)) else (1, doc)
      val start = Loc(offset, 0)
      buffer.replace(start, start + (rows, 0), nlines :+ Line.Empty)
      docExpanded = !docExpanded
    }

    def visit () {
      project.codex.visit(editor, view, df)
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
    case Kind.MODULE => Styles(CodeConfig.preprocessorStyle)
    case Kind.TYPE   => Styles(CodeConfig.typeStyle)
    case Kind.FUNC   => Styles(CodeConfig.functionStyle)
    case Kind.VALUE  => Styles(CodeConfig.variableStyle)
    case _           => Styles.None
  }
}
