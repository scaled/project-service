//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

/** A trait implemented by auxiliary project major modes to allow passing the project on to minor
  * modes. I'm not thrilled with this approach, so more thinking is here needed.
  */
trait HasProjectMode {

  def project :Project
}
