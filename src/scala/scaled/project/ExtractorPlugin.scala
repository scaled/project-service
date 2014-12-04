//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.extract.Extractor
import scaled._

/** Allows packages to provide Codex extractors for different languages. */
abstract class ExtractorPlugin extends AbstractPlugin {

  /** The set of file suffixes for which this plugin can create extractors. */
  val suffs :Set[String]

  /** Creates an extractor for files of type `suff` in `project`, if possible.
    * `suff` is guaranteed to be a member of [[suffs]]. */
  def extractor (project :Project, suff :String) :Option[Extractor]
}
