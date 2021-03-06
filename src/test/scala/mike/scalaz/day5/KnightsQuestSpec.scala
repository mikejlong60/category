package mike.scalaz.day5

import org.scalatest.{FlatSpec, Matchers}

import scalaz.Scalaz._

class KnightsQuestSpec extends FlatSpec with Matchers {

  trait TestContext {
    case class KnightPos(c: Int, r: Int) {
      def move: List[KnightPos] =
        for {
          KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
            KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
            KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
            KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (((1 |-> 8) contains c2) && ((1 |-> 8) contains r2))
        } yield KnightPos(c2, r2)

      def in3: List[KnightPos] =
        for {
          first <- move
          second <- first.move
          third <- second.move
        } yield third

      def canReachIn3(end: KnightPos): Boolean = in3 contains end

    }

  }

  "Knights Quest move function" must "reach a position in three moves" in new TestContext {
    val r = KnightPos(6, 2).move
    r should be(List(KnightPos(8,1), KnightPos(8,3), KnightPos(4,1), KnightPos(4,3), KnightPos(7,4), KnightPos(5,4)))
  }

  "Knights Quest in3 function" must "deterime whether or not you can reach a given position from a given position in three moves" in new TestContext {
    ( KnightPos(6,2) canReachIn3 KnightPos(6,1)) should be (true)

    ( KnightPos(6,2) canReachIn3 KnightPos(8,2)) should be (false)

  }

}
