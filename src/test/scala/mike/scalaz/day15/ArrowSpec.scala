package mike.scalaz.day15

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class ArrowSpec extends WordSpecLike with Matchers {

  "Arrow" must {
    "compose 2 functions into one using <<< (compose)" in {
      val f = (_: Int) + 1
      val g = (_: Int) * 100
      val a = (f <<< g)(2)
      a should be(201)
    }

    "compose 2 functions into one using >>> (andThen)" in {
      val f = (_: Int) + 1
      val g = (_: Int) * 100
      val a = (f >>> g)(2)
      a should be(300)
    }

    "use *** to combine two arrows into a pair of values: one " +
      "on the first item of the pair and one on the second " +
      "item of the pair" in {
        val f = (_: Int) + 1
        val g = (_: Int) * 100
        val a = (f *** g)(1, 2)
        a should be(2, 200)
      }

    "use &&& to combine two arrows into a new arrow by running the " +
      " two arrows on the same value.  Arrow is useful when " +
      "you need to add some context to functions and pairs." in {
        val f = (_: Int) + 1
        val g = (_: Int) * 100
        val a = (f &&& g)(2)
        a should be(3, 200)
      }

  }
}
