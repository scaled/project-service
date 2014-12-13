//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.{Def, Kind}
import java.nio.file.Path
import scaled._
import scaled.util.Errors

/** Provides an interface for interacting with test frameworks. */
abstract class Tester (project :Project) extends AutoCloseable {

  /** A value used to capture (and reinvoke) the most recent test invocation. */
  val lastTest = OptValue[RBufferView => Unit]()

  /** Returns the buffer in which we record test output. It will be created if needed. */
  def buffer () :Buffer = project.createBuffer(s"*test:${project.name}*", "log" /*project-test*/)

  /** Frees any resources maintained by this instance. */
  def close () {} // nada by default

  /** Aborts any currently active test session. */
  def abort () :Unit = throw Errors.feedback("This tester does not support aborting tests.")

  /** Locates the test file that's associated with the specified source file. */
  def findTestFile (file :Path) :Option[Path] = None

  /** Returns true if `df` represents a test function. By default all functions are considered so,
    * but a tester may wish to refine this notion. */
  def isTestFunc (df :Def) :Boolean = df.kind == Kind.FUNC

  /** Runs all tests in the project. Test output will be directed to [[buffer]].
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-all-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runAllTests (window :Window, interact :Boolean) :Boolean

  /** Runs all tests in `file`. If available, model information for all types (classes) in that
    * compilation unit will also be provided, to make life easier for the test framework. Test
    * output will be directed to [[buffer]].
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runTests (window :Window, interact :Boolean, file :Path, types :Seq[Def]) :Boolean

  /** Runs a single test in `file`. The test to be run is identified by `elem`. This is only ever
    * invoked interactly. Test output will be directed to [[buffer]].
    * @return a future which will be completed when the test completes.
    */
  def runTest (window :Window, file :Path, elem :Def) :Future[Unit]

  /** Reports the results of a test run. */
  protected def noteResults (window :Window, interact :Boolean, succs :Int, fails :Seq[Visit]) {
    window.visits() = new Visit.List("test failure", fails)
    if (interact) {
      val msg = s"Test run completed; $succs succeeded, ${fails.size} failed."
      window.emitStatus(msg)
    }
  }
}
