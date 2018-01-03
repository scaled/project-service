//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import java.util.function.Consumer
import scaled._
import scaled.pacman.Config
import scaled.util.{Close, MoreFiles}

/** Creates clients for language servers. Should be tagged as `langserver`. */
abstract class LangPlugin extends AbstractPlugin {

  /** The set of file suffixes handled by this language server.
    * @param root the root directory of the project in which the langserver will operate. */
  def suffs (root :Path) :Set[String]

  /** Returns whether this language server can be activated in the supplied project `root`. If a
    * language server requires configuration files to exist, this is the place to check. */
  def canActivate (root :Path) :Boolean

  /** Creates a language client for the supplied `project`. */
  def createClient (project :Project) :Future[LangClient]
}
