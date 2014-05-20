//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import java.util.zip.ZipFile
import scaled._

/** Exposes the contents of a zip file as a project. This is dumb like [[FileProject]], but can at
  * least do project-wide file completion using all the files in the zip file.
  */
class ZipFileProject (val zipPath :Path, metaSvc :MetaService) extends Project(metaSvc) {
  import scala.collection.convert.WrapAsScala._

  def root = zipPath

  private val entries = Seq() ++ new ZipFile(zipPath.toFile).entries.map(_.getName)
  val fileCompleter = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = {
      val matches = entries.filter(startsWithI(prefix))
      sortedCompletion(matches.map(new ZipEntryStore(zipPath, _)), _.name)
    }
  }

  def name = root.getFileName.toString
}
