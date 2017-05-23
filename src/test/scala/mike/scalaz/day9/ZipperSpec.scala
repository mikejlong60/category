package mike.scalaz.day9

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz._, Scalaz._, scala.language.higherKinds

class ZipperSpec extends WordSpecLike with Matchers {

  trait TestContext {

    val x = Stream(1, 2, 3, 4)

  }

  "Zipper" must {
    "allow you to move around the Zipper" in new TestContext {
      val a = x.toZipper
      val e = x.toZipper >>= { _.next } >>= { _.previous }
      a should be(e)
    }

    "allow you to modify the Zipper using flatMap(>>=)" in new TestContext {
      val a = x.toZipper >>= { _.next } >>= { _.modify { _ => 7 }.some }
      val e = Stream(1, 7, 3, 4).toZipper >>= { _.next }
      a.get.toList should be(e.get.toList)
    }

    "allow you to modify the Zipper using a for comprehension" in new TestContext {
      val a = for {
        z <- x.toZipper
        n1 <- z.next
        n2 <- n1.next
      } yield { n2.modify { _ => 7 } }

      val e = Stream(1, 2, 7, 4).toZipper
      a.get.toList should be(e.get.toList)
    }
  }
}
