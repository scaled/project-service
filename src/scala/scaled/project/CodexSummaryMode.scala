//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model._
import codex.store.ProjectStore
import java.util.Optional
import java.util.function.Predicate
import scaled._

object CodexSummaryMode {

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
      Store.scratch(tgt.name, tgt.project.root.path),
      tgt.project.bufferState("codex-summary", tgt), true)
    win.focus.visit(buf)
  }
}

@Major(name="codex-summary",
       desc="""A major mode that displays a summary of a def and its members.""")
class CodexSummaryMode (env :Env, tgt :CodexSummaryMode.Target) extends CodexReadingMode(env) {
  import CodexSummaryMode._

  val psvc = env.msvc.service[ProjectService]
  val project = Project(buffer)
  import project.pspace
  lazy val stores = pspace.codex.stores(project)

  override def keymap = super.keymap.
    bind("zoom-out",      "o", "<").
    bind("zoom-in",       "i", ">").
    bind("show-docs",     "SPACE").
    bind("visit",         "v", ".").
    bind("visit-or-zoom", "ENTER");

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

  @Fn("Displays the full documentation for the def at the point.")
  def showDocs () :Unit = act(view.point(), _.showDocs())

  //
  // Implementation details

  class Info extends Line.Tag {
    def zoomIn () {}
    def visit () {}
    def visitOrZoom () {}
    def showDocs () {}
    override def key :Any = classOf[Info]
  }
  private val NoInfo = new Info() {
    override def toString = "NoInfo"
  }

  private def act (p :Loc, fn :Info => Unit) = fn(buffer.line(p).lineTag(NoInfo))

  if (buffer.start == buffer.end) {
    val docr = new DocReader()

    def add (defs :Iterable[Def]) {
      // group defs by access, then within access sort them by flavor, then name
      val byAcc = defs.groupBy(_.access)
      var wroteAcc :Access = null
      for (acc <- Access.values) {
        byAcc.get(acc) foreach { defs =>
          for (mem <- defs.toSeq.sortBy(d => (d.flavor, d.name))) {
            if (mem.kind != Kind.SYNTHETIC) {
              // defer the writing of the access separator until we *actually* write a def with
              // this access level; otherwise we could write the level and then discover that all
              // the defs in it are invisible (synthetic)
              if (acc != Access.PUBLIC && acc != wroteAcc) {
                buffer.append(Seq(Line(s" ${acc.toString.toLowerCase}:")))
                buffer.split(buffer.end)
                wroteAcc = acc
              }
              addDefInfo(mem, docr, "  ")
            }
          }
        }
      }
    }

    tgt match {
      case TopLevelMembers(store) => // otherwise show the top-level members
        addProjectInfo(store)
        add(store.topLevelDefs)

      case DefMembers(df) => // if we have a def, show it and its members
        def addParent (df :Def) :Unit = if (df != null) {
          addParent(df.outer)
          addDefInfo(df, docr, "")
          // tack an extra blank line after the module parent
          if (df.kind == Kind.MODULE) buffer.split(buffer.end)
        }
        addProjectInfo(df.project)

        if (df.kind == Kind.TYPE) {
          // enumerate all members of this def and its supertypes, and group the members by the
          // supertype that defines them
          val supers = OO.linearizeSupers(stores, df)
          val trueJ = new Predicate[Def] { def test (df :Def) = true } // TODO: SAM
          val byOwner = OO.resolveMethods(supers, trueJ).groupBy(_.outer)
          for (sdf <- supers ; mems <- byOwner.get(sdf)) {
            if (sdf == df) addParent(df)
            else { buffer.split(buffer.end) ; addDefInfo(sdf, docr, "") }
            add(df.members.filter(m => m.kind == Kind.TYPE || m.kind == Kind.VALUE) ++ mems)
          }
        } else {
          addParent(df)
          add(df.members)
        }
    }
    view.point() = Loc.Zero
  }

  private def addProjectInfo (store :ProjectStore) {
    val title = s"project ${store.name}"
    buffer.append(Seq(Line(title), Line("-" * title.length)))
    buffer.split(buffer.end)
    buffer.split(buffer.end)
  }

  private def addDefInfo (df :Def, docr :DocReader, indent :String) {
    val fmt = CodexUtil.resolveDoc(psvc, stores, docr, df)
    val summary :SeqV[LineV] = fmt.summary(indent, view.width()-1)
    val sig :Seq[LineV] = df.sig match {
      case sigO if (!sigO.isPresent) => Seq(Line(s"$indent<no sig: $df>"))
      case sigO => CodexUtil.formatSig(sigO.get, indent)
    }

    // start by appending our summary and signature to the buffer
    var loc = buffer.end
    buffer.split(buffer.append(summary ++ sig))
    val end = buffer.end

    // tag all inserted lines with our info
    val info = new Info() {
      override def zoomIn () = pspace.codex.summarize(window, view, df)
      override def visit () = pspace.codex.visit(window, view, df)
      override def visitOrZoom () = df.kind match {
        case Kind.MODULE | Kind.TYPE if (Some(df) != tgt) => zoomIn()
        case _ => visit()
      }
      override def showDocs () = view.popup() = CodexUtil.mkDefPopup(env, stores, df, end)
      override def toString = "Def($df)"
    }
    while (loc < end) { buffer.setLineTag(loc, info) ; loc = loc.nextL }
  }
}
