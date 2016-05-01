//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.major.ReadingMode

@Major(name="codex-info",
       desc="""A major mode for displaying Codex information.""")
class CodexReadingMode (env :Env) extends ReadingMode(env) {

  // we use the code mode styles even though we're not a code mode
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
}
