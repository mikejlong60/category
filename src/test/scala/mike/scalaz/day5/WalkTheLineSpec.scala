package mike.scalaz.day5

import scalaz._, Scalaz._
import org.scalatest.{Matchers, WordSpecLike}

class WalkTheLineSpec extends WordSpecLike with Matchers {

  trait TestContext {
    type Birds = Int

    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] = if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none

      def landRight(n: Birds): Option[Pole] = if (math.abs((right + n) - left) < 4) copy(right = right + n).some
      else none

      def banana: Option[Pole] = none

    }
  }

  "Pierre" must {
    "balance as long as either side differs by less than four birds. " in new TestContext {
      Pole(0, 0).landLeft(2) flatMap { _.landRight(2) } should be(Some(Pole(2, 2)))
    }
    "not balance if a side differs by more than four birds." in new TestContext {
      Pole(0, 0).landLeft(12) flatMap { _.landRight(2) } should be(empty)
    }
    "slip and fall if he steps on the banana even if balanced. " in new TestContext {
      Pole(0, 0).landLeft(2) flatMap { _.landRight(2) } flatMap { _.banana } should be(empty)
    }
    "make functions that ignore their input parameter and just return a predetermined monadic value, in this case none from none >> 3.some" in  {
      (none: Option[Int]) >> 3.some should be(empty)
    }
    "make functions that ignore their input parameter and just return a predetermined monadic value, in this case 3.some from 4.some >> 3.some" in {
      4.some >> 3.some should be(3.some)
    }
    "make functions that ignore their input parameter and just return a predetermined monadic value, in this case none from 4.some >> none" in {
      4.some >> none should be(empty)
    }
    "use `for` syntax with some options" in {
      val s = for {
        x <- 3.some
        y <- "hi".some
      } yield (x.shows + y)
      s should be (Some("3hi"))
    }
    "use `for` syntax with a tightrope walker routine" in new TestContext {
      val routine = for {
        start <- Monad[Option].point(Pole(0,0))
        first <- start.landLeft(2)
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
      routine should be (Some(Pole(3,2)))
    }
    "use `for` syntax with a tightrope walker routine where the guy falls" in new TestContext {
      val routine = for {
        start <- Monad[Option].point(Pole(0,0))
        first <- start.landLeft(2)
        oops <- (none: Option[Pole])
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
      routine should be (empty)
    }
    "use pattern matching in `for` syntax" in {
      val justH = for {
        (x :: xs) <- "hello".toList.some
      } yield x
      justH should be (Some('h'))
    }
    "show pattern matching failure in `for` syntax" in {
      val no = for {
        (x :: xs) <- "".toList.some
      } yield x
      no should be (none)
    }
  }
}
