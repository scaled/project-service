//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Element
import java.util.TreeMap
import java.util.function.Consumer
import scaled._

object SourceIndex {

  /** Creates a source index builder. */
  def builder (store :Store) :Builder = new Builder(store)

  /** Used to build [[SourceIndex]] instances. */
  class Builder (store :Store) extends Consumer[Element] {
    private val lineOffsets = readLineOffsets(store)
    private val rows = new Array[Row](lineOffsets.size)

    override def accept (elem :Element) {
      lineOffsets.floorEntry(elem.offset) match {
        case null => println(s"No row entry for elem offset: $elem")
        case rowent =>
          val rowoff = rowent.getKey ; val row = rowent.getValue
          (rows(row) match {
            case null => val r = new Row() ; rows(row) = r ; r
            case r    => r
          }).add(elem.offset-rowoff, elem)
      }
    }

    /** Builds the source index based on the consumed elements. */
    def build () :SourceIndex = new SourceIndex(store, rows)
  }

  private class Row {
    val elems = new TreeMap[Int,Element]()

    def add (col :Int, elem :Element) {
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

  private def readLineOffsets (store :Store) = {
    val offsets = new TreeMap[Int,Int]()
    store.read { r =>
      val buf = new Array[Char](1024)
      var off = 0 ; var row = 0 ; var read = 0 ; var sawEOL = true
      while (read >= 0) {
        read = r.read(buf)
        if (read > 0) {
          var ii = 0; while (ii < read) {
            val c = buf(ii)
            if (c != '\r' && sawEOL) {
              offsets.put(off+ii, row)
              row += 1
              sawEOL = false
            }
            if (c == '\n') sawEOL = true
            ii += 1
          }
          off += read
        }
      }
    }
    offsets
  }
}

/** Contains a metadata index for a source file. The metadata is obtained from Codex and then
  * organized in a way that makes it efficiently available to Scaled.
  * @param store the store from which the source code came.
  */
class SourceIndex (val store :Store, rows :Array[SourceIndex.Row]) {

  /** Returns the element at the specified source location, if any. */
  def elementAt (loc :Loc) :Option[Element] = rows(loc.row) match {
    case null => None
    case row  => row.elementAt(loc.col)
  }
}
