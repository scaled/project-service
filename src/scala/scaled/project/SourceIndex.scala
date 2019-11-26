//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Element, Def, Kind}
import java.util.{Map, TreeMap}
import java.util.function.Consumer
import scaled._

object SourceIndex {

  /** Creates a source index builder. */
  def builder (store :Store) :Builder = new Builder(store)

  /** Used to build [[SourceIndex]] instances. */
  class Builder (store :Store) extends Consumer[Element] {
    private val lineOffsets = readLineOffsets(store)
    private val rows = Array.fill(lineOffsets.size)(new Row())
    private val encls = new TreeMap[Int,Def]()

    override def accept (elem :Element) :Unit = {
      lineOffsets.floorEntry(elem.offset) match {
        case null => println(s"No row entry for elem offset: $elem")
        case rowent =>
          val rowoff = rowent.getKey ; val row = rowent.getValue
          rows(row).add(elem.offset-rowoff, elem)
      }
      elem match {
        case df :Def if (df.kind != Kind.VALUE) => encls.put(df.offset, df)
        case _ => // nada
      }
    }

    /** Builds the source index based on the consumed elements. */
    def build () :SourceIndex = new SourceIndex(store, rows, encls)
  }

  private class Row {
    lazy val elems = new TreeMap[Int,Element]()

    def add (col :Int, elem :Element) :Unit = {
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
class SourceIndex (val store :Store, rows :Array[SourceIndex.Row], encls :TreeMap[Int,Def]) {

  /** Returns the element at the specified source location, if any. */
  def elementAt (loc :Loc) :Option[Element] =
    if (loc.row >= rows.length) None else rows(loc.row).elementAt(loc.col)

  /** Returns all elements on `row` in occurrence order. */
  def elements (row :Int) :Iterable[Element] = rows(row).elems.values

  /** Returns the def that encloses the specified character offset, if any. */
  def encloser (offset :Int) :Option[Def] = encls.floorEntry(offset) match {
    case null => None
    case ent  =>
      def encloses (df :Def) = offset >= df.offset && offset <= df.bodyEnd
      def check (ent :Map.Entry[Int,Def]) :Option[Def] = {
        if (ent == null) None
        else if (encloses(ent.getValue())) Some(ent.getValue())
        else check(encls.lowerEntry(ent.getKey()))
      }
      check(ent)
  }
}
