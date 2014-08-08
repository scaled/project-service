//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Doc}
import scaled.code.CodeConfig
import scaled.util.BufferBuilder

/** Handles formatting documentation. TODO: make this a plugin which is resolved based on the file
  * suffix of the source file that contains the to-be-documented element.
  */
trait DocFormatter {

  /** Formats the documentation for `df` into `bb`. */
  def formatDoc (df :Def, doc :Doc, text :String, bb :BufferBuilder) :Unit
}

object DocFormatter {

  val Default = new DocFormatter() {
    def formatDoc (df :Def, doc :Doc, text :String, bb :BufferBuilder) {
      val accum = new StringBuilder()
      def flush () :Unit = if (accum.length > 0) {
        val text = accum.toString
        // add a paragraph separator if appropriate
        if (!bb.lines.isEmpty && !text.startsWith("@") && bb.lines.last.length > 0) bb.addBlank()
        bb.addFilled(text, CodeConfig.docStyle)
        accum.setLength(0)
      }
      text.split(System.lineSeparator) foreach { rline =>
        val line = trimDocEnd(trimDocStart(rline.trim))
        // accumulate lines until we reach a blank line or a line starting with <p> or @
        if (line.length == 0 || line.startsWith("<p>") || line.startsWith("@")) flush()
        if (accum.length > 0) accum.append(' ')
        accum.append(line)
      }
      flush()
    }
  }

  def trimDocStart (line :String) =
    if (line.startsWith("/**")) line.substring(3).trim
    else if (line.startsWith("*")) line.substring(1).trim
    else line

  def trimDocEnd (line :String) =
    if (line.endsWith("*/")) line.substring(0, line.length-2).trim else line
}
