//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** Provides an interface for interacting with test frameworks. */
abstract class Tester (project :Project) extends AutoCloseable {

  /** The latest set of test failures as a navigable ring. */
  def failures :ErrorRing = _fails

  /** Returns the buffer in which we record test output. It will be created if needed. */
  def buffer (editor :Editor) :Buffer = editor.createBuffer(
    s"*test:${project.name}*", true, ModeInfo("log" /*project-test*/, Nil)).buffer

  /** Frees any resources maintained by this instance. */
  def close () {} // nada by default

  /** Runs all tests in the project. Test output will be directed to [[buffer]].
    * @param interactive if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-all-on-save`.
    */
  def runAllTests (editor :Editor, interactive :Boolean) :Unit

  /** Runs all tests in `file`. If available, model information for all types (classes) in that
    * compilation unit will also be provided, to make life easier for the test framework. Test
    * output will be directed to [[buffer]].
    * @param interactive if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-on-save`.
    */
  def runTests (editor :Editor, file :Path, typess :Seq[Model.Element], interactive :Boolean) :Unit

  /** Runs a single test in `file`. The test to be run is identified by `elem`. This is only ever
    * invoked interactively. Test output will be directed to [[buffer]].
    */
  def runTest (editor :Editor, file :Path, elem :Model.Element) :Unit

  /** Reports the results of a test run. */
  protected def noteResults (editor :Editor, interactive :Boolean, succs :Int, fails :Seq[Error]) {
    _fails = failureRing(fails)
    if (interactive) {
      val msg = s"Test run completed; $succs succeeded, ${fails.size} failed."
      editor.emitStatus(msg)
    }
  }

  private def failureRing (fails :Seq[Error]) = new ErrorRing("failure", fails)

  private[this] var _fails = failureRing(Seq())
}
