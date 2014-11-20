//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Def
import scaled._
import scaled.major.ReadingMode

object CodexFindUsesConfig extends Config.Defs {

  /** The CSS style applied to file paths. */
  val pathStyle = "usePathStyle"

  /** The CSS style applied to line numbers. */
  val lineNoStyle = "useLineNoStyle"

  /** The CSS style applied to uses. */
  val matchStyle = EditorConfig.matchStyle // standard matchStyle
}

@Major(name="codex-find-uses", tags=Array("project"),
       desc="""A major mode that displays all known uses of a def.""")
class CodexFindUsesMode (env :Env, df :Def) extends ReadingMode(env) {
  import CodexFindUsesConfig._

  val project = Project(buffer)
  import project.pspace

  override def configDefs = CodexFindUsesConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/codex.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("visit-use", "ENTER");

  case class VisitTag (visit :Visit) extends Line.Tag
  private val noUse = VisitTag(new Visit() {
    protected def go (window :Window) = window.popStatus("No use on the current line.")
  })

  @Fn("Visits the use on the current line.")
  def visitUse () {
    buffer.line(view.point()).lineTag(noUse).visit(window)
  }

  var visitList :Visit.List = _

  // look up our uses in the background and append them to the buffer
  if (buffer.start == buffer.end) env.exec.runInBG {
    println(s"Finding uses of $df")
    val visits = Seq.builder[Visit]()
    project.store.usesOf(df).toMapV foreach { (src, offsets) =>
      println(s"$src -> ${offsets.length}")
      val lines = Seq.builder[Line]()
      val srcstr = src.toString
      lines += Line.builder(srcstr).withStyle(pathStyle, 0, srcstr.length).build()
      val srcdef = (df.source == src)
      val store = PSpaceCodex.toStore(src)
      def offat (oo :Int) = if (oo < offsets.length) offsets(oo) else -1
      var oo = 0
      store.readLines { (line, offset) =>
        def add (fileoff :Int) :Boolean = {
          val lineoff = fileoff - offset
          if (lineoff < 0 || lineoff >= line.length) false
          else {
            val visit = Visit(store, fileoff)
            lines += Line.builder(line).
              withStyle(matchStyle, lineoff, lineoff+df.name.length).
              withLineTag(VisitTag(visit)).
              build()
            visits += visit
            true
          }
        }
        while (add(offat(oo))) oo += 1
        if (srcdef) add(df.offset)
      }
      env.exec.runOnUI {
        buffer append lines
        buffer split buffer.end
      }
    }

    env.exec.runOnUI {
      visitList = new Visit.List("use", visits.build())
      window.visits() = visitList
      view.point() = Loc.Zero
    }
  }
  // reinstate our visit list if our buffer is already generated
  else if (visitList != null) window.visits() = visitList
}
