//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.store.MapDBStore
import java.nio.file.Path
import scaled._
import scaled.util.BufferBuilder

/** Used to report info about this store with its project. */
class CodexComponent (store :CodexStore) extends Project.Component {

  /** Appends a description of this store to `bb`. */
  def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Codex:")
    bb.add(Line.builder(s"Defs: ${store.defCount}").withLineTag(Visit.Tag(new Visit() {
      protected def go (window :Window) = CodexSummaryMode.visitTopLevel(window, store)
    })).build())
  }

  def close () {} // not used
}

/** A [[ProjectStore]] with some extra integration with Scaled bits. */
class CodexStore (val root :Project.Root, indexFile :Path) extends MapDBStore(
  root.toString, indexFile) {

  /** Returns true if this store has no defs. */
  def isEmpty () = !topLevelDefs.iterator.hasNext
}
