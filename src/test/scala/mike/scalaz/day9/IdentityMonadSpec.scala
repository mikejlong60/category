package mike.scalaz.day9

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class IdentityMonadSpec extends WordSpecLike with Matchers {

  "Id" must {
    "all data types can be Id of the type" in {
      val a = (0: Id[Int])
      val e = 0
      a should be(e)
    }

    "have squared function" in {
      val a = (2: Id[Int])
      val e = (a, a)
      a.squared should be(e)
    }

    "have left function" in {
      val a = (2: Id[Int])
      val e = \/.left(a)
      a.left should be(e)
    }

    "write function application at the end of an expression" in {
      val a = 1 + 2 + 3 |> {x => x.point[List]}
      val e = List(6)
      a should be(e)
    }

    "have visit function which applies the id when function is not applied" in {
      val a = 12 visit {case x @ (2|3) => List(x * 12)}
      val e = List(12)
      a should be (e)
    }

    "have visit function which applies a partial function" in {
      val a = 3 visit {case x @ (2|3) => List(x * 12)}
      val e = List(36)
      a should be (e)
    }
  }
}
