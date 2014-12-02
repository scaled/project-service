//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import org.junit._
import org.junit.Assert._
import scaled._
import scaled.util.BufferBuilder

class DocFormatterTest {
  import DocFormatterPlugin._

  @Test def testFiller () {
    val bb = new BufferBuilder(80)
    val fill = new DocFiller("..", bb)
    fill.para()
    fill.add("Now is the time for all good men to come to the aid of their country. ", "a")
    fill.add("Every good boy deserves fudge.")
    fill.list("* ", "")
    fill.add("Who was that man?")
    fill.list("* ", "")
    fill.add("Now here's a little story, I like to tell, ")
    fill.add("about three bad brothers, you know so well.")
    fill.pre()
    fill.add("public class Foo {")
    fill.add("  public void main (String[] args) {")
    fill.add("  }")
    fill.add("}")
    fill.para()
    fill.add("It started way back, in history, with Adrock, MCA, and me, Mike D.")
    fill.para()

    val expect = Seq(
      "..Now is the time for all good men to come to the aid of their country. Every",
      "..good boy deserves fudge.",
      "",
      "..* Who was that man?",
      "..* Now here's a little story, I like to tell, about three bad brothers, you",
      "..  know so well.",
      "",
      "..public class Foo {",
      "..  public void main (String[] args) {",
      "..  }",
      "..}",
      "",
      "..It started way back, in history, with Adrock, MCA, and me, Mike D.")

    expect.zip(bb.lines.map(_.asString)) foreach { case (want, got) => assertEquals(want, got) }
  }
}
