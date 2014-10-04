//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._
import scaled.util.Errors

/** Provides an interface for interacting with test frameworks. */
abstract class Tester (project :Project) extends AutoCloseable {

  /** The latest set of test failures as a navigable ring. */
  def failures :ErrorRing = _fails

  /** Returns the buffer in which we record test output. It will be created if needed. */
  def buffer (editor :Editor) :Buffer = editor.bufferConfig(s"*test:${project.name}*").
    reuse().mode("log" /*project-test*/).tags("project").state(project.asState).create().buffer

  /** Frees any resources maintained by this instance. */
  def close () {} // nada by default

  /** Aborts any currently active test session. */
  def abort () :Unit = throw Errors.feedback("This tester does not support aborting tests.")

  /** Locates the test file that's associated with the specified source file. */
  def findTestFile (file :Path) :Option[Path] = None

  /** Runs all tests in the project. Test output will be directed to [[buffer]].
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-all-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runAllTests (editor :Editor, interact :Boolean) :Boolean

  /** Runs all tests in `file`. If available, model information for all types (classes) in that
    * compilation unit will also be provided, to make life easier for the test framework. Test
    * output will be directed to [[buffer]].
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runTests (editor :Editor, interact :Boolean, file :Path, types :Seq[Model.Element]) :Boolean

  /** Runs a single test in `file`. The test to be run is identified by `elem`. This is only ever
    * invoked interactly. Test output will be directed to [[buffer]].
    */
  def runTest (editor :Editor, file :Path, elem :Model.Element) :Unit

  /** Reports the results of a test run. */
  protected def noteResults (editor :Editor, interact :Boolean, succs :Int, fails :Seq[Error]) {
    _fails = failureRing(fails)
    if (interact) {
      val msg = s"Test run completed; $succs succeeded, ${fails.size} failed."
      editor.emitStatus(msg)
    }
  }

  private def failureRing (fails :Seq[Error]) = new ErrorRing("failure", fails)

  private[this] var _fails = failureRing(Seq())
}
