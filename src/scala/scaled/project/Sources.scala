//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.SourceSet
import com.google.common.collect.HashMultimap
import java.nio.file.Path
import java.util.function.Consumer
import scaled._
import scaled.util.{BufferBuilder, MoreFiles}

/** A component of a project that provides access to source files.
  * @param dirs all top-level directories which contain source code.
  */
class Sources (val dirs :SeqV[Path]) extends Project.Component {

  /** Applies `op` to all source files in this project.
    * @param forTest if true `op` is applied to the test sources, if false the main sources. */
  def onSources (op :Consumer[Path]) :Unit = MoreFiles.onFiles(dirs, op)

  /** Returns a map of all source files in this project, grouped by file suffix. */
  def summarize :Map[String,SourceSet] = {
    val bySuff = HashMultimap.create[String,Path]()
    onSources { file =>
      val fname = file.getFileName.toString
      fname.lastIndexOf(".") match {
        case -1 => // skip it!
        case ii => bySuff.put(fname.substring(ii+1), file)
      }
    }
    val mb = Map.builder[String,SourceSet]()
    bySuff.asMap.entrySet.toSetV foreach { ent =>
      mb += (ent.getKey, SourceSet.create(ent.getValue, ent.getValue.size))
    }
    mb.build()
  }

  override def describeSelf (bb :BufferBuilder) {
    if (!dirs.isEmpty) {
      bb.addSubHeader("Sources")
      bb.addKeysValues("Dirs: " -> dirs.mkString(" "))
      val srcsum = summarize
      if (!srcsum.isEmpty) {
        bb.addSection("Files by suffix:")
        bb.addKeysValues(srcsum.map((suff, srcs) => (s".$suff: ", srcs.size.toString)))
      }
    }
  }

  override def close () {}
}
