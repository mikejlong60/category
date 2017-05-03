package mike.scalaz.day7

import org.scalatest.{Matchers, WordSpecLike}
import scalaz._
import Scalaz._
import scala.language.higherKinds

class EitherAndValidationSpec extends WordSpecLike with Matchers {

  "For Either we" must {
    "demonstrate the purpose of right." in {
      val a: scalaz.\/[String, Int] = 1.right[String]
      val e: scalaz.\/[String, Int] = \/-(1)
      a should be(e)
    }

    "demonstrate the purpose of left." in {
      val a: scalaz.\/[String, Int] = "error".left[Int]
      val e: scalaz.\/[String, Int] = -\/("error")
      a should be(e)
    }

    "demonstrate that its not a monad unless you use the right projection" in {
      val a = Left[String, Int]("boom").right flatMap { x => Right[String, Int](x + 1) }
      val e = Left("boom")
      a should be(e)
    }

    "demonstrate using it in a for comprehension" in {
      val a = for {
        e1 <- "event 1 ok".right
        e2 <- "event 2 failed".left[String]
        e3 <- "event 3 failed".left[String]
      } yield (e1 |+| e2 |+| e3)
      val e = -\/("event 2 failed")
      a should be(e)
    }

    "demonstrate that you know right from left" in {
      "event 1".right.isRight should be(true)
      "event 1".right.isLeft should be(false)
    }

    "demonstrate getOrElse and its symbolic alias |" in {
      "event 1".right | "bad" should be("event 1")
      "event 1".left | "bad" should be("bad")
    }

    "demonstrate how you can use map to modify the right side" in {
      val a: scalaz.\/[Nothing, String] = "event1".right map { s => s"$s dude!" }
      val e: scalaz.\/[Nothing, String] = \/-("event1 dude!")
      a should be(e)
    }

    "demonstrate how you can't use map to modify the left side unless you use the right projection. And that seems pretty worthless" in {
      val a = ("boom").left.right map { x: scalaz.\/[String, String] => Right[String, String]("ff") }
      val e = \/-(Right("ff"))
      a should be(e)
    }

    "demonstrate how to chain on the left side using orElse or its alias |||" in {
      val a = ("event1 failed").left ||| "retry event1 ok".right
      val e = \/-("retry event1 ok")
      a should be(e)
      val a2 = ("event1 failed").left orElse "retry event1 ok".right
      a2 should be(e)
    }
  }

  "For Validation we" must {

    "understand the use of success" in {
      val a = "event1 ok".success
      val e = Success("event1 ok")
      a should be(e)
    }

    "understand the use of failure" in {
      val a = "event1 ok".failure
      val e = Failure("event1 ok")
      a should be(e)
    }

    "understand how to chain success and failure but you are appending all the failures together into a single string which seems dumb." in {
      val a = ("event1 ok. ".success[String] |@| "event2 failed. ".failure[String] |@| "event3 failed. ".failure[String]) { (x, y, z) => List(x, y, z) }
      val e = Failure("event2 failed. event3 failed. ")
      a should be(e)
    }

    "understand how not to wrap a value into a non empty list" in {
      val a = 1.wrapNel
      val e = NonEmptyList(1)
      a should be(e)
    }

    "understand how not to wrap success and failure values into a non empty list" in {
      val a = ("event1 ok.".successNel[String] |@| "event2 failed.".failureNel[String] |@| "event3 failed.".failureNel[String] |@| "event4 failed.".failureNel[String] ) {_ + _ + _ + _}//{ (x, y, z, q) => List(x, y, z) }
      val e = Failure(NonEmptyList("event2 failed.","event3 failed.","event4 failed."))
      a should be(e)
    }
  }
}
