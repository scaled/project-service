//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.store.MapDBStore
import java.nio.file.Path
import scaled._
import scaled.util.BufferBuilder

/** A [[ProjectStore]] with some extra integration with Scaled bits. */
class CodexStore (val root :Project.Root, indexFile :Path) extends MapDBStore(
  root.toString, indexFile) with Project.Component {

  /** Returns true if this store has no defs. */
  def isEmpty () = !topLevelDefs.iterator.hasNext

  /** Appends a description of this store to `bb`. */
  def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Codex:")
    bb.add(Line.builder(s"Defs: $defCount").withLineTag(Visit.Tag(new Visit() {
      protected def go (window :Window) = CodexSummaryMode.visitTopLevel(window, CodexStore.this)
    })).build())
  }
}
