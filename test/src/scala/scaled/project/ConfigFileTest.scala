//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Paths
import org.junit._
import org.junit.Assert._
import scaled._

class ConfigFileTest {

  @Test def testReadWriteConfig () :Unit = {
    val config = Seq(
      Seq("foo", "bar", "baz dingleberry quux"),
      Seq("who", "was", "that", "man"),
      Seq("now is the time"),
      Seq("one", "two", "five!", "three, sir", "three!"))

    val path = Paths.get(System.getProperty("java.io.tmpdir")).resolve("test.tmp")
    // println(path)
    ConfigFile.write(path, config)
    assertEquals(config, ConfigFile.read(path))
  }
}
