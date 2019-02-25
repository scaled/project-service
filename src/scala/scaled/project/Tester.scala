//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import codex.model.Kind
import java.nio.file.Path
import scaled._
import scaled.util.Errors

/** Provides an interface for interacting with test frameworks. */
abstract class Tester (project :Project) extends Project.Component {

  /** A value used to capture (and reinvoke) the most recent test invocation. */
  val lastTest = OptValue[RBufferView => Unit]()

  /** The buffer into which to append test results. */
  def resultsBuffer :Buffer = project.logBuffer

  /** Aborts any currently active test session. */
  def abort () :Unit = throw Errors.feedback("This tester does not support aborting tests.")

  /** Locates the test file that's associated with the specified source file. */
  def findTestFile (file :Path) :Option[Path] = None

  /** Finds the test function (if any) in the supplied list of definitions that "enclose" the
    * point. These definitions are obtained from the language intel when the user asks to "run the
    * test at the point".
    * @param defns the definitions that enclose a point in the buffer, from inner-most to
    * outer-most. */
  def findTestFunc (defns :Ordered[Intel.Defn]) :Option[Intel.Defn] =
    defns.find(_.kind == Kind.FUNC)

  /** Runs all tests in the project.
    * @param window a window to pass to `noteResults`. *.
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-all-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runAllTests (window :Window, interact :Boolean) :Boolean

  /** Runs all tests in `file`.
    * @param window a window to pass to `noteResults`. *.
    * @param interact if true the user manually requested this test run, if false, it was
    * triggered as a result of `retest-on-save`.
    * @return false if we know immediately that there are no tests to run, true otherwise.
    */
  def runTests (window :Window, interact :Boolean, file :Path) :Boolean

  /** Runs a single test in `file`. The test to be run is identified by `name`. This is only ever
    * invoked interactively.
    * @param window a window to pass to `noteResults`. *.
    * @return a future which will be completed with `this` the test completes.
    */
  def runTest (window :Window, file :Path, defn :Intel.Defn) :Future[Tester]

  /** Reports the results of a test run. */
  protected def noteResults (window :Window, interact :Boolean, succs :Int, fails :SeqV[Visit]) {
    window.visits() = new Visit.List("test failure", fails)
    if (interact) {
      val msg = s"Test run completed; $succs succeeded, ${fails.size} failed."
      window.emitStatus(msg)
    }
  }
}
