//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.File
import java.nio.file.{Path, Paths}

/** Some shared Maven-related utilities. */
object Maven {
  import Project._

  /** The user's local Maven repository. */
  val m2repo = Paths.get(System.getProperty("user.home"), ".m2", "repository")

  /** Resolves the `.pom` file for `id`. */
  def resolvePOM (id :RepoId) = resolve(id, "pom")

  /** Resolves the binary `.jar` file for `id`. */
  def resolveClasses (id :RepoId) = resolve(id, "jar")

  /** Resolves the sources `.jar` file for `id`. */
  def resolveSources (id :RepoId) = resolve(id, "jar", Some("sources"))

  /** Resolves the `.m2` `.ext` file for `id`. */
  def resolve (id :RepoId, ext :String, classifier :Option[String] = None) :Path = {
    val csuff = classifier.map(c => s"-$c").getOrElse("")
    val artifactName = s"${id.artifactId}-${id.version}$csuff.$ext"
    m2repo.resolve(id.groupId.replace('.', File.separatorChar)).resolve(id.artifactId).resolve(
      id.version).resolve(artifactName)
  }
}
