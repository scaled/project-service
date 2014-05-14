//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.io.File
import java.util.Date
import reactual.{Future, Value, ValueV}
import scala.annotation.tailrec
import scaled._
import scaled.util.Error

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project {

  /** Returns the name of this project. */
  def name :String

  /** Returns the root of this project. */
  def root :File

  /** Returns a unique identifier for this project, if one can be determined. Generally this is
    * Maven-style: `groupId:name:version`, but a `Project` is welcome to use whatever makes sense
    * for the kind of projects it manages.
    */
  def id :Option[String] = None

  /** Returns the version control source URL for this project, if one can be determined. This should
    * be prefixed with the version control type if it is not already naturally part of the URL:
    *
    * - git:https://github.com/scaled/maven-project.git
    * - git:git@github.com:samskivert/samskivert.git
    * - hg:https://ooo-maven.googlecode.com/hg/
    * - svn:https://ooo-gwt-utils.googlecode.com/svn
    */
  def sourceURL :Option[String] = None

  /** Notes that buffer is now using this project. */
  def reference (buffer :Buffer) :this.type = {
    _refcount += 1
    this
  }

  /** Notes that buffer is no longer using this project. */
  def release (buffer :Buffer) {
    assert(_refcount > 0, s"$this released with zero refcount!")
    _refcount -= 1
    if (_refcount == 0) hibernate()
  }

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Completes files in this project. The string representation of the files should not be
    * prefixed with path information, but rather suffixed and only where necessary to avoid
    * name collisions.
    *
    * Thus one might see as possible completions: `Bar.scala Baz.scala(util/) Baz.scala(data/)`
    * When completing on `Ba`.
    */
  val fileCompleter :Completer[File]

  /** Returns the compiler that handles compilation for this project. Created on demand. */
  def compiler :Compiler = {
    if (_compiler == null) _compiler = createCompiler()
    _compiler
  }

  override def toString = s"Project($root, $name, $id, $sourceURL)"

  /** When a project is released by all project modes, it goes back into hibernation. This method
    * should shut down any complex services maintained by the project. The default implementation
    * shuts down any active compiler.
    */
  protected def hibernate () {
    if (_compiler != null) {
      _compiler.shutdown()
      _compiler = null
    }
  }

  /** Creates and returns a new compiler instance. By default a NOOP compiler is created. */
  protected def createCompiler () :Compiler = new Compiler(this) {
    override def shutdown () {}
    override def recompile (editor :Editor, interactive :Boolean) {
      if (interactive) editor.emitStatus("Compilation is not supported by this project.")
    }
    override protected def compile (buffer :Buffer) = Future.success(true)
    override protected def nextError (buffer :Buffer, start :Loc) = None
  }

  private[this] var _refcount = 0 // see reference()/release()
  private[this] var _compiler :Compiler = null // see compiler()/hibernate()
}
