//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Doc}
import scaled._
import scaled.code.CodeConfig
import scaled.util.BufferBuilder

/** Handles formatting documentation. Plugins implementing this API should tag themselves with
  * `doc-formatter`.
  * @param suffs the file suffixes identifying the source files handled by this formatter.
  */
abstract class DocFormatterPlugin (val suffs :String*) extends AbstractPlugin {

  /** Creates a [[Format]] instance for `df`'s `doc`.
    * @param text the raw doc text extracted from the source file. */
  def format (df :Def, doc :Doc, text :String) :DocFormatterPlugin.Format
}

object DocFormatterPlugin {

  /** Generates formatted documentation for a single def. */
  abstract class Format {
    /** Formats the (first sentence) doc summary into `bb`. */
    def summary (indent :String, bb :BufferBuilder) :Unit
    /** Formats the full docs into `bb`. */
    def full (indent :String, bb :BufferBuilder) :Unit

    /** Formats and returns the (first sentence) doc summary. */
    def summary (indent :String, fillWidth :Int) :Seq[LineV] = fmt(fillWidth, summary(indent, _))
    /** Formats and returns the full docs. */
    def full (indent :String, fillWidth :Int) :Seq[LineV] = fmt(fillWidth, full(indent, _))

    private def fmt (fillWidth :Int, fn :BufferBuilder => Unit) :Seq[LineV] = {
      val bb = new BufferBuilder(fillWidth)
      fn(bb)
      bb.lines
    }
  }

  val NoDoc = new Format {
    def summary (indent :String, bb :BufferBuilder) = full(indent, bb)
    def full (indent :String, bb :BufferBuilder) =
      bb.add(s"${indent}Undocumented", CodeConfig.docStyle)
  }

  val Default = new DocFormatterPlugin() {
    def format (df :Def, doc :Doc, text :String) = new Format() {
      def summary (indent :String, bb :BufferBuilder) = text.indexOf(System.lineSeparator) match {
        case -1 => format(text, indent, bb)
        case ii => format(text.substring(0, ii), indent, bb)
      }
      def full (indent :String, bb :BufferBuilder) = format(text, indent, bb)
      private def format (text :String, indent :String, bb :BufferBuilder) {
        text.split(System.lineSeparator) foreach { l => bb.add(s"$indent$l", CodeConfig.docStyle) }
      }
    }
  }
}
