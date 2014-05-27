//
// Scaled Project Service - a Scaled framework for grokking projects.
// http://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import scaled.{Line, LineV, Store}

/** Defines a simple model of code. Code is modeled as a tree of nested elements. An element:
  *
  *  - has a simple name (e.g. `PeanutFactory`, `time_t`, `foo`, `getName`, `get-name`)
  *  - has a location in a source file (file and character offset)
  *  - is enclosed by zero or one elements
  *  - encloses zero or more elements
  *  - is uniquely identified by the path of simple names from its outermost enclosing element to
  *    the element itself
  *  - has a kind (e.g. `module`, `type`, `func`, `term`)
  *  - has a visibility (`public` or `non-public`)
  *  - has zero or more language-specific attributes (e.g. a Java `type` may be tagged as a
  *    `class`, `interface`, `enum`, or `annotation`)
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

  /** Locates a code element in a source file.
    * @param file the compilation unit that contains this element.
    * @param start the character offset in `file` at which this element is defined. */
  case class Location (file :Store, start :Int)

  /** Defines the different kinds of elements. */
  sealed trait Kind
  /** An assembly of modules. Examples: a `jar` file, a C# `dll` assembly, a source project. */
  case object Assembly extends Kind
  /** A namespaced collection of types, functions and terms. Examples: a Java/C#/Scala package, a
    * Scala object, a C++ namespace. */
  case object Module extends Kind
  /** A named type, with type, function and term members. Examples: a Java/C#/Scala/C++ class or
    * interface, a C struct. */
  case object Type extends Kind
  /** A function, procedure or method. */
  case object Func extends Kind
  /** A named value. Examples: a Java/C#/C++ class field, a C struct member, a function parameter, a
    * local variable. */
  case object Term extends Kind

  /** The default set of kinds to seek when searching. */
  val findKinds :Set[Kind] = Set(Module, Type, Func, Term)

  /** Defines a code element. */
  case class Element (
    /** This element's kind. */
    kind  :Kind,
    /** The simple name of this element. */
    name  :String,
    /** The path to this element. This is composed of the simple names of all elements that enclose
      * this element (excepting `Element.Root`) in inner-most to outer-most. This element's name is
      * not included in the path. */
    path  :List[String],
    /** Whether or not this element is public. */
    isPub :Boolean,
    /** Zero or more language specific attributes. */
    attrs :Array[String] = NoAttrs
  ) {
    /** Returns this element's path combined with spaces. */
    lazy val pathString = path.mkString(" ")
  }

  /** An empty array for elements with no attributes. */
  val NoAttrs = new Array[String](0)

  /** The root element, used as the parent for all top-level elements. */
  val Root = Element(Assembly, "<root>", Nil, true)
}
