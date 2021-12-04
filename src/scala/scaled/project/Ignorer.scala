//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import java.util.regex.Pattern
import scaled._

/** Used to ignore files in projects. */
abstract class Ignorer {

  /** Returns true if `path` should be ignored. */
  def apply (path :Path) :Boolean

  /** Returns a meaningful description of this ignorer. */
  def toString :String
}

/** Helpers for `Ignorer` component. */
object Ignorer {

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

  /** Returns an ignorer for a specific path. The path must be absolute.
    * @param relativeTo a path to which _path is relative (usually the project root), used when
    * showing the ignore in the project description. */
  def ignorePath (_path :Path, relativeTo :Path) = new Ignorer {
    if (!_path.isAbsolute) throw new IllegalArgumentException(
      s"Must provide absolute path to ignorePath. Got: ${_path}")
    override def apply (path :Path) = path == _path
    override def toString = s"${relativeTo.relativize(_path)}/"
  }

  /** The standard set of directories that are ignored when enumerating all project dirs. */
  def stockIgnores :SeqBuffer[Ignorer] = SeqBuffer(
    dotfileIgnorer, ignoreName(".git"), ignoreName(".hg"), ignoreName(".svn")) // TODO: more?

  /** Parses `.gitignore` and `.git/info/exclude` relative to `root` (if they exist) and creates
    * ignorers for their contents. */
  def gitIgnores (root :Path) :Seq[Ignorer] = {
    var ignorers = SeqBuffer[Ignorer]()
    def add (file :Path) :Unit = {
      if (Files.exists(file)) for (line <- Files.readAllLines(file)) {
        if (line.startsWith("#")) {} // comment
        else if (line.contains("/")) ignorers += ignorePath(root.resolve(line), root)
        else ignorers += ignoreName(line)
      }
    }
    add(root.resolve(".gitignore"))
    add(root.resolve(".git/info/exclude"))
    ignorers.toSeq
  }
}
