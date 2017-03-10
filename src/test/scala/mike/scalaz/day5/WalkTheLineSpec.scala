package mike.scalaz.day5

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scala.language.higherKinds
import scalaz.scalacheck.ScalazProperties.functor
import org.scalacheck.{Gen, Arbitrary}
import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._, scalacheck.ScalaCheckBinding._
import org.scalatest.{Matchers, WordSpecLike}

class WalkTheLineSpec extends WordSpecLike with Matchers {

  trait TestContext {
    type Birds = Int

    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] = if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none

      def landRight(n: Birds): Option[Pole] = if (math.abs((right + n) - left) < 4) copy(right = right + n).some
      else none

    }
  }

  "Pierre" must {
    "balance as long as either side differs by less than four birds. " in new TestContext {
      Pole(0, 0).landLeft(2) flatMap { _.landRight(2) } should be(Some(Pole(2, 2)))
    }
    "not balance if a side differs by more than four birds." in new TestContext {
      Pole(0, 0).landLeft(12) flatMap { _.landRight(2) } should be(empty)
    }
  }
}
