//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.LinkOption
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.BufferBuilder

/** A simple project used when we can't identify anything other than the root directory of the
  * project. This will be used if we see a `.git`, `.hg`, etc. directory or some other indicator
  * of the root of a project.
  */
final class FileProject (val root :Path, msvc :MetaService) extends AbstractFileProject(msvc) {
  override def isIncidental = true
  override def name = root.getFileName.toString
}

/** A base class for projects that are rooted in a directory. Provides a completer over all files in
  * the directory tree.
  */
abstract class AbstractFileProject (msvc :MetaService) extends Project(msvc) {

  private class Dir (dir :Path) {
    import scala.collection.convert.WrapAsScala._
    var files = Set[Path]()
    var dirs = Set[Path]()
    var lastRefresh = FileTime.fromMillis(0L)

    def refresh () {
      val lm = Files.getLastModifiedTime(dir)
      if (lm.compareTo(lastRefresh) > 0) {
        lastRefresh = lm
        val fs = Seq() ++ Files.list(dir).iterator
        val (nd, nf) = fs.partition(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
        val nfiles = nf.filter(Files.isRegularFile(_)).toSet
        if (files != nfiles) {
          files = nfiles
          _allFiles = null
        }
        val ndirs = nd.filterNot(ignore).map(_.toRealPath()).toSet
        if (ndirs != dirs) {
          _allFiles = null
          // remove directories that have gone away
          (dirs -- ndirs) foreach { dirMap -= _ }
          // add new directories
          val newdirs = (ndirs -- dirs)
          newdirs foreach { d => dirMap += (d -> new Dir(d)) }
          // and refresh any directories that have changed
          (ndirs -- newdirs) map(dirMap) foreach { _.refresh() }
          // finally update our cached directories
          dirs = ndirs
        }
        // println(s"Rebuilt $dir (files=${files.size} dirs=${dirs.size})")
      }
      // refresh our children
      dirs map(dirMap) foreach { _.refresh() }
    }
  }
  private val dirMap = MMap[Path,Dir](root -> new Dir(root))

  private var _allFiles :Set[Path] = _
  private def allFiles = {
    dirMap(root).refresh()
    if (_allFiles == null) {
      _allFiles = Set() ++ dirMap.values.flatMap(_.files)
      // println(s"Rebuilt all files map (size: ${_allFiles.size})")
    }
    _allFiles
  }

  val fileCompleter = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = {
      sortedCompletion(allFiles.filter(fileFilter(prefix)).map(Store.apply), f => defang(f.name))
    }
  }

  override def describeSelf (bb :BufferBuilder) {
    super.describeSelf(bb)
    bb.addSubHeader("Files")
    bb.addKeysValues("files: " -> allFiles.size.toString,
                     "ignores: " -> ignores.mkString(" "))
  }

  protected def ignore (dir :Path) :Boolean = {
    val name = dir.getFileName.toString
    name.startsWith(".") || ignores(name)
  }
  protected def ignores :Set[String] = FileProject.stockIgnores
}

object FileProject {

  /** The standard set of directories that are ignored when enumerating all project dirs. */
  val stockIgnores = Set(".git", ".hg", ".svn") // TODO: more

  /** Creates file projects rooted at .git directories. */
  @Plugin(tag="project-finder")
  class GitFinderPlugin extends FinderPlugin("git") {
    def checkRoot (root :Path) = if (Files.isDirectory(root.resolve(".git"))) 1 else -1
  }

  /** Creates file projects rooted at .hg directories. */
  @Plugin(tag="project-finder")
  class MercurialFinderPlugin extends FinderPlugin("mercurial") {
    def checkRoot (root :Path) = if (Files.isDirectory(root.resolve(".hg"))) 1 else -1
  }

  /** Creates file projects rooted at the highest .svn directory. */
  @Plugin(tag="project-finder")
  class SubversionFinderPlugin extends FinderPlugin("subversion") {
    def checkRoot (root :Path) = if (Files.isDirectory(root.resolve(".svn"))) 0 else -1
  }

  abstract class FinderPlugin (nm :String)
      extends ProjectFinderPlugin(nm, false, classOf[FileProject])
}
