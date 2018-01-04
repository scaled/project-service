//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.util.BufferBuilder

/** Identifies and manages this project's dependencies. */
abstract class Depends (project :Project) extends Project.Component {

  /** The ids of this project's dependencies, in highest to lowest precedence order. */
  def ids :SeqV[Project.Id]

  /** Information on any unresolvable dependencies. */
  def warnings :SeqV[String] = Seq.empty

  override def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Depends:")
    val deps = ids
    deps foreach { d =>
      bb.add(Line.builder(d.toString).withLineTag(Visit.Tag(new Visit() {
        protected def go (window :Window) = project.pspace.projectFor(d) match {
          case None => window.popStatus(s"Unable to resolve project for $d")
          case Some(p) => p.visitDescription(window)
        }
      })).build())
    }
    if (deps.isEmpty) bb.add("<none>")

    val ws = warnings
    if (!ws.isEmpty) {
      bb.addSection("Warnings:")
      ws foreach { bb.add(_) }
    }
  }
}
