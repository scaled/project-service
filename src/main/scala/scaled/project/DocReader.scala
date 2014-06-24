//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Doc, Source}
import java.io.Reader
import scala.collection.mutable.{Map => MMap}

/** Optimizes the process of extracting docs from source files. */
class DocReader {

  private val _cache = MMap[Source,Reader]()

  def resolve (source :Source, doc :Doc) :Array[String] = {
    // TODO: cache
    val r = source.reader()
    val buf = new Array[Char](doc.length)
    r.skip(doc.offset)
    r.read(buf)
    r.close()
    // TODO: trim leading whitespace up to start.col
    new String(buf).split(System.lineSeparator)
  }

  def close () {
    _cache.values.foreach(_.close())
  }
}
