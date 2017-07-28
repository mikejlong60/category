package mike.scalaz.day12

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class ApplicativeStyleSpec extends WordSpecLike with Matchers {

  "When using Monoidal Applicatives you" must {
    "be able to turn an Int monoid into an applicative" in {
      val a = Monoid[Int].applicative.ap3(1, -2, 4)(10)
      val e = 13
      a should be(e)
    }

    "be able to turn a List monoid into an applicative" in {
      val a = Monoid[List[Int]].applicative.ap3(List(1), List(-2), List(4))(List(12))
      val e = List(12, 1, -2, 4)
      a should be(e)
    }
  }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we make a product of List and Option" in {
      val a = Applicative[List].product[Option].point(1)
      val e = (List(1), Some(1))
      a should be(e)
    }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we append two tuples of (List, Option)" in {
      val a = ((List(1), 1.some) |@| (List(2), 2.some)) {
        _ |+| _
      }
      val e = (List(1, 2), Some(3))
      a should be(e)
    }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we append two tuples of (List, try), one try being a failure" in {
      val a = ((List(1), 1.success[String]) |@| (List(2), "boom".failure[Int])) {
        _ |+| _
      }
      val e = (List(1, 2), Failure("boom"))
      a should be(e)
    }

  "Unlike monads in general, applicative functors are closed under composition. Two" +
    "sequentially independent effects can be fused into one, their composition." +
    "Here we compose List and Option" in {
      val a = Applicative[List].compose[Option].point(10)
      val e = (List(10.some))
      a should be(e)
    }

  "The ⊗ and ⊙ operators allow you to combine applicative-style computations in" +
    "two different ways: parallel and sequential. Here we use traverse for a list, " +
    "the same as parallel in ScalaZ.  " must {
      "If the condition is not met the whole expression returns " +
        "none.  Traverse combines flatMap and sequence I think." in {

          val a = List(1, 2, 3) traverse { x => (x > 2) option (x + 1) }
          a should be(none)
        }

      "If the condition is met the expression returns a new list incrememented by the " +
        "function. Traverse combines flatMap and sequence I think." in {

          val a = List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }
          val e = Some(List(2, 3, 4))
          a should be(e)
        }

      "For a monoidal applicative functor, traversal accumulates values." in {
        val a = Monoid[Int].applicative.traverse(List(1, 2, 3)) { x => x + 1 }
        val e = 9
        a should be(e)
      }
    }
}
