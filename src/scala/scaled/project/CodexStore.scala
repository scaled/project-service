//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.store.MapDBStore
import java.nio.file.Path
import scaled._
import scaled.util.BufferBuilder

/** Used to report info about this store with its project. */
class CodexComponent (codex :Codex, store :CodexStore) extends Project.Component {

  override def addToBuffer (buffer :RBuffer) {
    // while this buffer is open, keep an up to date SourceIndex in its state
    val conn = codex.indexed.onValue { idx =>
      if (idx.store == buffer.store) buffer.state[SourceIndex]() = idx
    }
    buffer.killed.onEmit { conn.close() }
  }

  /** Appends a description of this store to `bb`. */
  override def describeSelf (bb :BufferBuilder) {
    bb.addSubHeader("Codex:")
    bb.add(Line.builder(s"Defs: ${store.defCount}").withLineTag(Visit.Tag(new Visit() {
      protected def go (window :Window) = CodexSummaryMode.visitTopLevel(window, store)
    })).build())
  }
}

/** A [[ProjectStore]] with some extra integration with Scaled bits. */
class CodexStore (val root :Project.Root, indexFile :Path) extends MapDBStore(
  root.toString, indexFile) {

  /** Returns true if this store has no defs. */
  def isEmpty () = !topLevelDefs.iterator.hasNext
}
