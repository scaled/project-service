//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled._

/** Provides metadata for a project's code. This metadata is in the form of [[Model]] information on
  * code [[Model.Element]]s.
  *
  * Methods which take an element will throw [[NoSuchElementException]] if they are passed an
  * element that is invalid, or no longer exists.
  */
abstract class Codex extends AutoCloseable {
  import Model._

  /** Called when this codex is no longer needed. This should terminate any external processes and
    * release any resources retained by this instance. */
  def close () {} // nada by default

  /** Returns the element identified by `path`. The path should be in inner-most to outer-most
    * order, in congruence with [[Element.path]].
    * @throws NoSuchElementException if no element can be found for `path`.
    */
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

  /** Finds all elements matching the following criteria:
    *  - kind is in `kinds`
    *  - name equals `name` (if `prefix` is false) or
    *  - name starts with `name` (if `prefix` is true)
    * Name comparison is done case-insensitively. */
  def find (kinds :Set[Kind], name :String, prefix :Boolean) :Seq[Element]

  /** A completer on the elements in this codex. */
  val completer :Completer[Element] = new Completer[Element]() {
    def complete (prefix :String) :Completion[Element] = prefix.split(":", 2) match {
      case Array(name, path) =>
        elemComp(find(findKinds, name.toLowerCase, false) filter(
          e => Completer.startsWithI(path)(e.pathString)))
      case Array(name) =>
        elemComp(find(findKinds, name.toLowerCase, true))
    }
    private def elemComp (es :Seq[Element]) = completion(es, elemToString)
    private val elemToString = (e :Element) => s"${e.name}:${e.pathString}"
  }
}
