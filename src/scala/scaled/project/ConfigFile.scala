//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Paths, Path}
import scaled._

/** Routines for dealing with very dumb config files that are just blank line separated blocks of
  * otherwise uninterpreted text.
  */
object ConfigFile {

  /** Reads `path` into a list of lists of strings. */
  def read (path :Path) :SeqV[SeqV[String]] = {
    val groups = Seq.builder[SeqV[String]]()
    val lines = Seq.view(Files.readAllLines(path))
    var start = 0
    var end = 0
    while (end < lines.size) {
      if (lines(end).length == 0) {
        if (end - start > 0) {
          groups += lines.slice(start, end)
        }
        start = end+1
      }
      end += 1
    }
    if (end - start > 0) {
      groups += lines.slice(start, end)
    }
    groups.build()
  }

  /** Writes the config `data` to `path` in a way that can be recovered by [[readConfig]]. */
  def write (path :Path, data :Ordered[SeqV[String]]) {
    val out = Files.newBufferedWriter(path)
    var first = true
    for (group <- data) {
      if (first) first = false
      else out.newLine() // blank separator
      for (line <- group) {
        out.write(line)
        out.newLine()
      }
    }
    out.close()
  }

  /** Reads `path` into a list of lists of strings and then turns that into a map where the first
    * string in each block is the key and the value is the remaining strings in the block. */
  def readMap (path :Path) :Map[String,SeqV[String]] = {
    val map = Map.builder[String,SeqV[String]]()
    for (group <- read(path)) {
      map.put(group(0), group.drop(1))
    }
    map.build()
  }

  /** Helper that makes it easy to write files that can be read using [[readMap]]. */
  class WriteMap (path :Path) {
    private val out = Files.newBufferedWriter(path)
    private var first = true

    def write (key :String, data :Iterable[String]) {
      if (first) first = false
      else out.newLine() // blank separator
      out.write(key)
      out.newLine()
      for (line <- data) {
        out.write(line)
        out.newLine()
      }
    }

    def close () {
      out.close()
    }
  }
}
