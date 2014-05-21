//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import java.util.zip.ZipFile
import scala.collection.mutable.{Map => MMap}
import scaled._

/** Exposes the contents of a zip file as a project. This is dumb like [[FileProject]], but can at
  * least do project-wide file completion using all the files in the zip file.
  */
class ZipFileProject (val zipPath :Path, metaSvc :MetaService) extends Project(metaSvc) {
  import scala.collection.convert.WrapAsScala._

  // map the zip entries into a faux file system
  abstract class Node {
    def insert (path :List[String]) :Unit
    def lookup (path :List[String]) :Iterable[String]
  }
  object FileNode extends Node {
    def insert (path :List[String]) = throw new IllegalStateException(path.toString)
    def lookup (path :List[String]) = if (path.isEmpty) Seq()
                                      else throw new IllegalArgumentException(path.toString)
  }
  class DirNode extends Node {
    val children = MMap[String,Node]()
    def insert (path :List[String]) :Unit = path match {
      case h :: t => if (t.isEmpty) children.put(h, FileNode)
                     else children.getOrElseUpdate(h, new DirNode()).insert(t)
      case _ => throw new IllegalArgumentException(path.toString)
    }
    def lookup (path :List[String]) = path match {
      case h :: t   =>
        if (t.isEmpty) children.keys else children.get(h) match {
          case Some(n) => n.lookup(t)
          case None    => Seq()
        }
      case _ => Seq()
    }
  }
  private lazy val rootNode = {
    val node = new DirNode()
    new ZipFile(zipPath.toFile).entries.foreach(
      e => if (!e.isDirectory) node.insert(splitPath(e.getName)))
    node
  }

  // turns foo/bar/baz into List("foo/", "bar/", "baz")
  private def splitPath (path :String) :List[String] = {
    val cs = path.split("/", -1)
    (cs.dropRight(1).map(c => s"$c/") :+ cs.last).toList
  }

  def root = zipPath
  def name = root.getFileName.toString

  val fileCompleter = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = {
      val comps = splitPath(prefix)
      val files = rootNode.lookup(comps)
      val matches = files.filter(startsWithI(comps.last))
      val pathpre = (if (prefix endsWith "/") comps else comps.dropRight(1)).mkString
      sortedCompletion(matches.map(m => ZipEntryStore(zipPath, pathpre + m)),
                       _.asInstanceOf[ZipEntryStore].entry)
    }
    override def pathSeparator = Some("/")
  }
}