//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind, Ref, Source}
import codex.store.{MapDBStore, ProjectStore, Query}
import java.util.{ArrayList, Optional, LinkedHashSet}
import scaled._
import scaled.util.FuzzyMatch

/** [[PSpaceCodex]] helpers and whatnot. */
object PSpaceCodex {

  /** Converts a `Source` to a `Store`. */
  def toStore (source :Source) :Store = Store(source.toString)
  // Source's toString representation is the same for expected by Store.apply

  /** Convert a `Store` to a `Source`. */
  def toSource (store :Store) :Source = store match {
    case fs :FileStore => new Source.File(fs.path.toString)
    case zes :ZipEntryStore => new Source.ArchiveEntry(zes.zipFile.toString, zes.entry)
    case _ => throw new IllegalArgumentException(s"Can't convert $store to Codex Source.")
  }
}

/** Integrates [[Codex]] more nicely with Scaled. */
class PSpaceCodex (pspace :ProjectSpace) extends AutoCloseable {
  import PSpaceCodex._

  /** The history rings for Codex completions. */
  val history = Map(Kind.MODULE -> new Ring(32),
                    Kind.TYPE   -> new Ring(32),
                    Kind.FUNC   -> new Ring(32),
                    Kind.VALUE  -> new Ring(32))

  override def close () {
    // TODO: anything?
  }

  /** Returns a completer on elements of `kind` in this project's Codex. */
  def completer (project :Project, kind :Kind) :Completer[Def] = new Completer[Def]() {
    override def minPrefix = 2
    def complete (glob :String) :Completion[Def] = {
      val elems = glob.split(":", 2) match {
        case Array(name      ) => query(name)
        case Array(name, path) => FuzzyMatch(path).filter(query(name), pathString)
      }
      Completion(glob, elems, false)(e => s"${e.name}:${pathString(e)}")
    }
    private def query (name :String) =
      (Query.prefix(name.take(minPrefix)) kind(kind) find(stores(project))).toSeqV
    private def pathString (d :Def) = d.qualifier
  }

  /** Resolves `ref`, which originated from a file in `project`. */
  def resolve (project :Project, ref :Ref) :Option[Def] =
    Option.from(Ref.resolve(stores(project), ref))

  /** Visits the source of `df` in a buffer in `window`. Pushes `curview` onto the visit stack. */
  def visit (window :Window, curview :BufferView, df :Def) {
    window.visitStack.push(curview) // push current loc to the visit stack
    val view = window.focus.visitFile(toStore(df.source))
    view.point() = view.buffer.loc(df.offset)
  }

  /** Displays a summary of `df` in a new buffer. Pushes `curview` onto the visit stack. */
  def summarize (window :Window, curview :BufferView, df :Def) {
    window.visitStack.push(curview) // push current loc to the visit stack
    CodexSummaryMode.visitDef(window, df)
  }

  /** Returns the store for `project`, stores for all top-level projects in this workspace, and
    * then all the stores for the dependencies of `project`. */
  def stores (project :Project) :LinkedHashSet[ProjectStore] = {
    // TODO: determine whether we want to cache these results and also whether we want to reference
    // the underlying projects...
    val stores = new LinkedHashSet[ProjectStore]()
    stores.add(project.store)
    pspace.allProjects map(_._1) map(pspace.projectIn) foreach { proj =>
      if (proj != project) stores.add(proj.store)
    }
    for (dep <- project.depends) project.depend(dep) match {
      case Some(p) => stores.add(p.store)
      case None => resolveNonProjectStore(dep) foreach(stores.add)
    }
    stores
  }

  /** Resolves the project store for a dependency for which a Scaled project was unavailable. If
    * `None` is returned, this dependency will be omitted from the Codex. */
  protected def resolveNonProjectStore (depend :Project.Id) :Option[ProjectStore] = None
}
