//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Source, Element}
import java.util.function.Consumer
import java.util.{Comparator, TreeSet, TreeMap}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled.{BufferV, Loc}

/** Contains a metadata index for a source file. The metadata is obtained from Codex and then
  * organized in a way that makes it efficiently available to Scaled.
  */
class SourceIndex (source :Source, buffer :BufferV) extends Consumer[Element] {

  /** Returns the element at the specified source location, if any. */
  def elementAt (loc :Loc) :Option[Element] = rows.get(loc.row).flatMap(_.elementAt(loc.col))

  // from interface Consumer[Element]
  override def accept (elem :Element) {
    val loc = buffer.loc(elem.offset)
    val row = rows.getOrElseUpdate(loc.row, new Row())
    row.add(elem, loc.col)
  }

  private class Row {
    val elems = new TreeMap[Int,Element]()

    def add (elem :Element, col :Int) {
      elems.put(col, elem)
    }

    def elementAt (col :Int) :Option[Element] = {
      val e = elems.floorEntry(col)
      if (e == null) None
      else {
        val ecol = e.getKey ; val elem = e.getValue
        // make sure the element overlaps the specified position
        if (ecol + elem.length >= col) Some(elem)
        else None
      }
    }
  }

  private val rows = MMap[Int,Row]()
}
