//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Doc}
import scala.collection.mutable.ArrayBuffer
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

  class DocFiller (indent :String, bb :BufferBuilder) {
    /** Puts the filler into 'accumulate paragraph' mode. Any currently accumulating block will be
      * closed. When a paragraph is closed, a blank line is inserted preceding it. */
    def para () :Unit = close(ParaM, "", "")

    /** Indicates the start of a list element with the specified prefix (e.g. '`- `'). Any currently
      * accumulating block will be closed. When a list element is closed, a blank line is inserted
      * only if the previous block was a non-list element. */
    def list (prefix :String, prefixStyle :String) :Unit = close(ListM, prefix, prefixStyle)

    /** Indicates the start of a preformatted block. Any currently accumulating block will be
      * closed. When the preformatted block is closed, a blank line is inserted preceding it. */
    def pre () :Unit = close(PreM, "", "")

    /** Adds `text` to the filler in the default style. For `para` and `list` blocks, the default
      * is `docStyle`, for `pre` blocks, the default is `textStyle`. */
    def add (text :String) :Unit = add(text, defaultStyle)

    /** Adds `text` to the filler in the specified `style`. */
    def add (text :String, style :String) :Unit = nextM match {
      case PreM => lines += new Line().addPre(text, style)
      case _    => val tlen = text.length ; if (tlen > 0) {
        if (lines.isEmpty) lines += new Line()
        var start = 0 ; while (start < tlen) {
          start = lines.last.add(start, text, style)
          if (start < tlen) lines += new Line()
        }
      }
    }

    // my kingdom for 'private:' syntax...
    private val fillWidth = bb.fillWidth
    private var listPre = ""
    private var listPreStyle = ""

    final private val ParaM = 1 ; final private val ListM = 2 ; final private val PreM = 3
    private var lastM = ParaM ; private var nextM = ParaM

    private val lines = ArrayBuffer[Line]()
    private class Line {
      val text = new java.lang.StringBuilder()
      val tags = new Tags()
      val scol = { // add our prefixes and compute the starting column
        text.append(indent)
        if (listPre.length > 0) {
          if (lines.isEmpty) { // we're the first line in this list block
            text.append(listPre)
            tags.add(listPreStyle, indent.length, text.length)
          } else {             // we're a subsequent line, so use whitespace
            text.append(" " * listPre.length)
          }
        }
        text.length
      }

      // for posterity: a little diagram of the cases in Line.add:
      // FOO BAR__________
      //        we want to append
      //                  ^ bpos < flen && isWhitespace(bpos) == true
      //                  add(frag.substr(start, bpos)) ; return bpos
      //        want to append
      //                  ^ bpos < flen && isWhitespace(bpos) == false
      //               ^ ipos <= scan back past last ws
      //               add(frag.substr(start, ipos) ; return ipos
      //        to append
      //                  ^ bpos >= flen
      //               add(frag.substr(start)) ; return flen

      def add (start :Int, frag :String, style :String) :Int = {
        val tlen = text.length ; val flen = frag.length
        // if text.length == scol, move start past leading whitespace
        @inline def skipws (ii :Int) :Int =
          if (ii == flen || !Character.isWhitespace(frag.charAt(ii))) ii else skipws(ii+1)
        var fstart = if (tlen == scol) skipws(start) else start
        val bpos = fstart+(fillWidth-tlen)
        def adduntil (end :Int) :Int = {
          if (end > fstart) {
            text.append(frag, fstart, end)
            tags.add(style, tlen, tlen+(end-fstart))
          }
          end
        }
        // if the (rest of the) whole fragment fits, add it
        if (bpos >= flen) adduntil(flen)
        // otherwise figure out where to break the fragment and add the first bit
        else {
          var end = bpos
          while (end > fstart && !Character.isWhitespace(frag.charAt(end))) end -= 1
          // if we found no whitespace, just hard break
          adduntil(if (end == fstart) bpos else end)
        }
      }

      def addPre (ptext :String, style :String) :this.type = {
        text.append(ptext)
        tags.add(style, 0, ptext.length)
        this
      }

      def toLine :LineV = {
        // TODO: trim trailing whitespace
        new Line.Builder(Line.toCharArray(text), tags).build()
      }
    }

    private def close (newMode :Int, newListPre :String, newListPreStyle :String) {
      if (!lines.isEmpty) {
        val wantBlank = nextM != ListM || lastM != ListM
        if (!bb.lines.isEmpty && wantBlank) bb.addBlank()
        bb.add(lines.map(_.toLine))
        lines.clear()
        lastM = nextM
      }
      nextM = newMode
      listPre = newListPre
      listPreStyle = newListPreStyle
    }

    private def defaultStyle = if (nextM == PreM) EditorConfig.textStyle else CodeConfig.docStyle
  }
}
