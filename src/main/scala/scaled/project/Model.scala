//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled.{LineV, Store}

/** Defines a simple model of code. Code is modeled as a tree of nested elements. An element has:
  *
  *  - a simple name (e.g. `PeanutFactory`, `time_t`, `foo`, `getName`, `get-name`)
  *  - a location in a source file (file and character offset)
  *  - a language-specific signature (an opaque string displayed to the user)
  *  - optional documentation (extracted from the source, like Javadoc)
  *  - is enclosed by zero or one elements
  *  - encloses zero or more elements
  *  - has a kind (one of `module`, `type`, `function`, `term`)
  *  - has a visibility (`public` or `non-public`)
  *  - has zero or more language-specific attributes (a Java `type` may be tagged as a `class`,
  *    `interface`, `enum`, or `annotation`)
  *
  * This classification attempts to do as little harm to the world's thousands of programming
  * languages, while still providing the foundation for useful navigation and visualization
  * mechanisms.
  *
  * Its goal is to provide some granularity to the user, who might think "I'm looking for a function
  * named `foo`" and doesn't want to hear about variables named `foo`, or thusly named structs,
  * classes, packages, etc. Thus when resolving ambiguity in translating a language into this model,
  * think of the user.
  *
  * Should a function in a language which supports first class functions be tagged as a function or
  * a term? Probably a function, even though it is technically also a term. You can be sure the
  * programmer is making that distinction in their head.
  *
  * Should a language which supports properties, which are backed by implicit getter and setter
  * functions, expose properties as functions or terms? Probably as terms, as the programmer likely
  * thinks of a property as a value, not as a function which generates the value.
  */
object Model {

  /** Defines the different kinds of elements. */
  sealed trait Kind
  object Module extends Kind
  object Type extends Kind
  object Function extends Kind
  object Term extends Kind

  /** Defines a code element. */
  abstract class Element {
    /** The simple name of this element. */
    def name  :String
    /** The compilation unit that contains this element. */
    def file  :Store
    /** The character offset in `file` at which this element is defined. */
    def start :Int
    /** This element's signature. The line may be styled. */
    def sig   :LineV
    /** This element's documentation, if any. The lines may be styled. */
    def doc   :Seq[LineV]
    /** This element's kind. */
    def kind  :Kind
    /** Whether or not this element is public. */
    def isPub :Boolean
    /** Zero or more language specific attributes. */
    def attrs :Seq[String]
  }
}
