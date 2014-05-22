//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled.LineV

/** Provides metadata for a project's code. This metadata is in the form of [[Model]] information on
  * code [[Model.Element]]s.
  */
abstract class Codex extends AutoCloseable {
  import Model._

  /** Returns the element identified by `path`. */
  def element (path :List[String]) :Element

  /** Returns the element that encloses `elem`. If `elem` is a root element, [[Root]] will be
    * returned. If [[Root]] is passed to this method, `IllegalArgumentException` will be thrown.
    */
  def parent (elem :Element) :Element =
    if (elem.path.isEmpty) throw new IllegalArgumentException()
    else element(elem.path.tail)

  /** Returns the signature of `elem`. The line may be styled. */
  def sig (elem :Element) :LineV

  /** Returns the documentation for `elem`, if any. The lines may be styled. */
  def doc (elem :Element) :Seq[LineV]

  /** Returns the location for `elem`, if it has one. Some elements like Java packages, do not have
    * a canonical source location. */
  def loc (elem :Element) :Option[Location]

  /** Returns the elements directly nested in `elem`. Supplying `Element.Root` should return all
    * top-level elements. */
  def members (elem :Element) :Seq[Element]

  /** Returns all elements whose kind is included in `kinds` and whose simple name starts with
    * `namePre`. */
  def find (kinds :Set[Kind], namePre :String) :Seq[Element]

  /** Called when this codex is no longer needed. This should terminate any external processes and
    * release any resources retained by this instance. */
  def close () {} // nada by default
}
