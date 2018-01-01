//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.LinkOption
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.HashMap
import java.util.function.Consumer
import java.util.regex.Pattern
import scaled._
import scaled.util.BufferBuilder

/** A simple project used when we can't identify anything other than the root directory of the
  * project. This will be used if we see a `.git`, `.hg`, etc. directory or some other indicator
  * of the root of a project.
  */
final class FileProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  override def isIncidental = true

  override protected def computeMeta (oldMeta :Project.Meta) = Future.success(oldMeta.copy(
    name = root.path.getFileName.toString
  ))
}

/** A base class for projects that are rooted in a directory.
  * Provides a completer over all files in the directory tree.
  */
abstract class AbstractFileProject (ps :ProjectSpace, r :Project.Root) extends Project(ps, r) {

  private class Dir (dir :Path) {
    var files = Set[Path]()
    var dirs = Set[Path]()
    var lastRefresh = FileTime.fromMillis(0L)

    def refresh () {
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
    dirMap.put(root.path, new Dir(root.path))
  }

  private def allDirs :Seq[Dir] = synchronized {
    dirMap.get(root.path).refresh()
    dirMap.values.toSeq
  }
  private def allFiles :Set[Path] = Set() ++ allDirs.flatMap(_.files)

  val fileCompleter = new Completer[Store]() {
    import Completer._
    def complete (prefix :String) = pspace.wspace.exec.runAsync(allFiles.map(Store.apply)).
      // prefix will always be "" here so we don't filter
      map(files => Completion(prefix, files, true)(f => defang(f.name)))
  }

  override def describeMeta (bb :BufferBuilder) {
    super.describeMeta(bb)
    bb.addSubHeader("Files")
    bb.addKeysValues("files: " -> allDirs.map(_.files.size).foldLeft(0)(_ + _).toString,
                     "ignores: " -> ignores().mkString(" "))
  }

  override def onFiles (op :Consumer[Path]) :Unit = allFiles foreach op.accept

  protected def ignore (dir :Path) :Boolean = ignores().exists(_(dir))
  protected val ignores = Value(FileProject.stockIgnores)
}

object FileProject {

  /** Used to ignore files in projects. */
  abstract class Ignorer {
    /** Returns true if `path` should be ignored. */
    def apply (path :Path) :Boolean
    /** Returns a meaningful description of this ignorer. */
    def toString :String
  }

  /** An ignorer for all files/dir with name that starts with `.`. */
  val dotfileIgnorer = new Ignorer {
    override def apply (path :Path) = path.getFileName.toString.startsWith(".")
    override def toString = ".*"
  }

  /** Returns an ignorer for files/dirs with `name`. */
  def ignoreName (name :String) = new Ignorer {
    override def apply (path :Path) = path.getFileName.toString == name
    override def toString = name
  }

  /** Returns an ignorer for files/dirs with name that match `regex`. */
  def ignoreRegex (regex :String) = new Ignorer {
    val pattern = Pattern.compile(regex)
    override def apply (path :Path) = pattern.matcher(path.getFileName.toString).matches
    override def toString = s"m/$regex/"
  }

  /** Returns an ignorer for a specific path.
    * @param relativeTo a path to which _path is relative (usually the project root), used when
    * showing the ignore in the project description. */
  def ignorePath (_path :Path, relativeTo :Path) = new Ignorer {
    override def apply (path :Path) = path == _path
    override def toString = s"${relativeTo.relativize(_path)}/"
  }

  /** The standard set of directories that are ignored when enumerating all project dirs. */
  def stockIgnores = SeqBuffer(dotfileIgnorer, ignoreName(".git"),
                               ignoreName(".hg"), ignoreName(".svn")) // TODO: more?

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

  abstract class FinderPlugin (nm :String) extends ProjectFinderPlugin(
    nm, false, classOf[FileProject])
}
