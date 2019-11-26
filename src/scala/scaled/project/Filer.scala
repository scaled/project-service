//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.LinkOption
import java.nio.file.attribute.FileTime
import java.nio.file.{Path, Files}
import java.util.HashMap
import java.util.function.Consumer
import java.util.zip.{ZipEntry, ZipFile}
import scaled._
import scaled.util.BufferBuilder

/** A component of a project that provides access to raw project files. Would be named `Files` but
  * that conflicts with `java.nio.file.Files` which results in a world of pain. */
abstract class Filer extends Project.Component {

  /** A completer on all (non-ignored) files in this project. */
  val completer :Completer[Store]

  /** Applies `op` to all (non-ignored) files in this project. */
  def onFiles (op :Consumer[Path]) :Unit

  override def close () :Unit = {}
}

/** A filer that obtains files from a directory on the file system. */
class DirectoryFiler (root :Path, exec :Executor, ignores :SeqV[Ignorer]) extends Filer {

  def this (project :Project, ignores :SeqV[Ignorer]) =
    this(project.root.path, project.pspace.wspace.exec, ignores)

  override val completer = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = exec.runAsync(allFiles.map(Store.apply)).
      // prefix will always be "" here so we don't filter
      map(files => Completion(prefix, files, true)(f => defang(f.name)))
  }

  override def onFiles (op :Consumer[Path]) :Unit = allFiles foreach op.accept

  override def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addSubHeader("Files")
    bb.addKeysValues("files: " -> allDirs.map(_.files.size).foldLeft(0)(_ + _).toString,
                     "ignores: " -> ignores.mkString(" "))
  }

  private class Dir (dir :Path) {
    var files = Set[Path]()
    var dirs = Set[Path]()
    var lastRefresh = FileTime.fromMillis(0L)

    def refresh () :Unit = {
      val lm = Files.getLastModifiedTime(dir)
      if (lm.compareTo(lastRefresh) > 0) {
        lastRefresh = lm
        val fs = { val stream = Files.list(dir)
                   try stream.iterator.toSeq finally stream.close() }
        val (nd, nf) = fs.partition(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
        val nfiles = nf.filterNot(ignore).filter(Files.isRegularFile(_)).toSet
        if (files != nfiles) {
          files = nfiles
        }
        val ndirs = nd.filterNot(ignore).map(_.toRealPath()).toSet
        if (ndirs != dirs) {
          // remove directories that have gone away
          (dirs -- ndirs) foreach dirMap.remove
          // add new directories
          val newdirs = (ndirs -- dirs)
          newdirs foreach { d => dirMap.put(d, new Dir(d)) }
          // and refresh any directories that have changed
          (ndirs -- newdirs) map(dirMap.get) foreach { _.refresh() }
          // finally update our cached directories
          dirs = ndirs
        }
        // println(s"Rebuilt $dir (files=${files.size} dirs=${dirs.size})")
      }
      // refresh our children
      dirs foreach { d => dirMap.get(d).refresh() }
    }
  }
  private val dirMap = new HashMap[Path,Dir]() ; {
    dirMap.put(root, new Dir(root))
  }

  private def allDirs :Seq[Dir] = synchronized {
    dirMap.get(root).refresh()
    dirMap.values.toSeq
  }
  private def allFiles :Set[Path] = Set() ++ allDirs.flatMap(_.files)

  protected def ignore (dir :Path) :Boolean = ignores.exists(_(dir))
}

/** A filer that obtains project files from one or more .zip files. */
class ZipFiler (zipPaths :Seq[Path]) extends Filer {

  val completer = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = Future.success({
      val comps = splitPath(prefix)
      val pathpre = (if (prefix endsWith "/") comps else comps.dropRight(1)).mkString
      val matches = rootNodes.flatMap { root =>
        root.lookup(comps).filter(startsWithI(comps.last)).map(
          m => ZipEntryStore(root.zipPath, pathpre + m))
      }
      Completion(prefix, matches, true)(_.asInstanceOf[ZipEntryStore].entry)
    })
    override def pathSeparator = Some("/")
  }

  override def onFiles (op :Consumer[Path]) :Unit = ???

  override def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addSubHeader("Files")
    // TODO
    // bb.addKeysValues("files: " -> allDirs.map(_.files.size).foldLeft(0)(_ + _).toString,
    //                  "ignores: " -> ignores().mkString(" "))
  }

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
}
