//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import java.util.function.Consumer
import java.util.zip.{ZipEntry, ZipFile}
import scaled._

/** Exposes the contents of one or more zip files as a project. This is dumb like [[FileProject]],
  * but can at least do project-wide file completion using all the files in the zip file(s).
  */
final class ZipFileProject (ps :ProjectSpace, val zipPaths :Seq[Path])
    extends AbstractZipFileProject(ps, Project.Root(zipPaths.head)) {
  def this (ps :ProjectSpace, zipPath :Path) = this(ps, Seq(zipPath))

  override def isIncidental = true

  override protected def computeMeta (oldMeta :Project.Meta) = Future.success(oldMeta.copy(
    name = root.path.getFileName.toString
  ))
}

/** A base class for projects that get their contents from a zip file. Provides a completer over all
  * entries in the zip file.
  */
abstract class AbstractZipFileProject (ps :ProjectSpace, r :Project.Root) extends Project(ps, r) {

  /** The zip files that make up this project. */
  val zipPaths :Seq[Path]

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
    val children = Mutable.cacheMap[String,Node] { dir => new DirNode() }
    def insert (path :List[String]) :Unit = path match {
      case h :: t => if (t.isEmpty) children.put(h, FileNode)
                     else children.get(h).insert(t)
      case _ => throw new IllegalArgumentException(path.toString)
    }
    def lookup (path :List[String]) = path match {
      case h :: t   =>
        if (t.isEmpty) children.asMap.keySet else children.getIfPresent(h) match {
          case null => Seq()
          case node => node.lookup(t)
        }
      case _ => Seq()
    }
  }
  class RootNode (val zipPath :Path) extends DirNode

  private lazy val rootNodes = zipPaths map { zipPath =>
    val node = new RootNode(zipPath)
    new ZipFile(zipPath.toFile).stream.forEach(new Consumer[ZipEntry]() {
      def accept (e :ZipEntry) = if (!e.isDirectory) node.insert(splitPath(e.getName))
    })
    node
  }

  // turns foo/bar/baz into List("foo/", "bar/", "baz")
  private def splitPath (path :String) :List[String] = {
    val cs = path.split("/", -1).mkSeq
    List.builder[String]().append(cs.dropRight(1).map(c => s"$c/")).append(cs.last).build()
  }

  val fileCompleter = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = {
      val comps = splitPath(prefix)
      val pathpre = (if (prefix endsWith "/") comps else comps.dropRight(1)).mkString
      val matches = rootNodes.flatMap { root =>
        root.lookup(comps).filter(startsWithI(comps.last)).map(
          m => ZipEntryStore(root.zipPath, pathpre + m))
      }
      Completion(prefix, matches, true)(_.asInstanceOf[ZipEntryStore].entry)
    }
    override def pathSeparator = Some("/")
  }
}
