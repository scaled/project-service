//
// Scaled Project Service - a Scaled framework for grokking projects.
// https://github.com/scaled/project-service/blob/master/LICENSE

package scaled.project

import java.nio.file.Paths
import org.junit._
import org.junit.Assert._
import scaled._

class CodecTest {
  import Project._
  import Codec._

  @Test def testReadShowRoot () :Unit = {
    def test (root :Root) :Unit = {
      val str = showRoot(root)
      assertEquals(root, readRoot(str))
    }
    test(Root(Paths.get("/foo/bar/baz")))
    test(Root(Paths.get("/foo/bar/baz"), "test"))
    test(Root(Paths.get("c:\\monkey\\butter"), "pickle-sauce"))
  }

  @Test def testReadShowId () :Unit = {
    def test (id :Id) :Unit = {
      val str = showId(id)
      // println(s"$id --> $str")
      assertEquals(id, readId(str).get)
    }
    test(RepoId(MavenRepo, "com.foo.bar", "baz", "22.5-SNAPSHOT"))
    test(SrcURL("git", "https://github.com/foobar/baz"))
    test(SrcURL("git", "git:ssh:github.com/foobar/baz"))
    test(SrcURL("hg", "ssh:peanut@bitbucket.org/popcorn/candy"))
    test(PlatformId("jdk", "tools"))
  }
}
