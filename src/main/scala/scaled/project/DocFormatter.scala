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
    def para () :Unit = close(ParaM, "")

    /** Indicates the start of a list element with the specified prefix (e.g. '`- `'). Any currently
      * accumulating block will be closed. When a list element is closed, a blank line is inserted
      * only if the previous block was a non-list element. */
    def list (prefix :String) :Unit = close(ListM, prefix)

    /** Indicates the start of a preformatted block. Any currently accumulating block will be
      * closed. When the preformatted block is closed, a blank line is inserted preceding it. */
    def pre () :Unit = close(PreM, "")

    /** Adds `text` to the filler in the default style. For `para` and `list` blocks, the default
      * is `docStyle`, for `pre` blocks, the default is `textStyle`. */
    def add (text :String) :Unit = add(text, defaultStyle)

    /** Adds `text` to the filler in the specified `style`. */
    def add (text :String, style :String) {
      nextM match {
        case PreM =>
          lines += new Line()
          lines.last.addPre(text, style)
        case _ =>
          if (lines.isEmpty) lines += new Line()
          @inline def loop (start :Int) {
            val end = lines.last.add(start, text, style)
            if (end < text.length) {
              lines += new Line()
              loop(end)
            }
          }
          loop(0)
      }
    }

    // my kingdom for 'private:' syntax...
    private val fillWidth = bb.fillWidth-indent.length
    private var listPre = ""
    private def curFillWidth = fillWidth - listPre.length

    final private val ParaM = 1 ; final private val ListM = 2 ; final private val PreM = 3
    private var lastM = ParaM ; private var nextM = ParaM

    private val lines = ArrayBuffer[Line]()
    private class Line {
      val text = new java.lang.StringBuilder()
      val tags = new Tags()

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
        // if text.length == 0, move start past leading whitespace
        @inline def skipws (ii :Int) :Int =
          if (ii == flen || !Character.isWhitespace(frag.charAt(ii))) ii else skipws(ii+1)
        var fstart = if (tlen == 0) skipws(start) else start
        val bpos = fstart+(curFillWidth-tlen)
        def adduntil (end :Int) :Int = {
          text.append(frag, fstart, end)
          tags.add(style, tlen, tlen+(end-fstart))
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

      def addPre (text :String, style :String) {
        this.text.append(text)
        tags.add(style, 0, text.length)
      }

      def toLine (pre :String) :LineV = {
        val pp = pre.length ; val tt = text.length
        val chars = new Array[Char](pp+tt)
        pre.getChars(0, pp, chars, 0)
        var ii = 0 ; while (ii < tt) { chars(pp+ii) = text.charAt(ii) ; ii += 1 }
        // TODO: trim trailing whitespace
        new Line.Builder(chars, tags).build()
      }
    }

    private def close (newMode :Int, newListPre :String) {
      if (!lines.isEmpty) {
        val wantBlank = nextM != ListM || lastM != ListM
        if (!bb.lines.isEmpty && wantBlank) bb.addBlank()
        bb.add(lines.head.toLine(s"$indent$listPre"))
        val repre = indent + (" " * listPre.length)
        bb.add(lines.tail.map(_.toLine(repre)))
        lines.clear()
        lastM = nextM
      }
      nextM = newMode
      listPre = newListPre
    }

    private def defaultStyle = if (nextM == PreM) EditorConfig.textStyle else CodeConfig.docStyle
  }
}
