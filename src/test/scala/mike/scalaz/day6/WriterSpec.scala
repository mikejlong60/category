package mike.scalaz.day6

import org.scalatest.{FlatSpec, Matchers, WordSpecLike}

import scalaz.{Monad, Monoid, Writer}
import scalaz.Scalaz._

class WriterSpec extends WordSpecLike with Matchers {

  trait TestContext {
    def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9")

    def logNumber(x: Int): Writer[List[String], Int] = x.set(List(s"Got number: ${x.shows}"))

    def multWithLog: Writer[List[String], Int] = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b

    implicit class PairOps[A, B: Monoid](pair: (A, B)) {
      def applyLog[C](f: A => (C, B)): (C, B) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log |+| newlog)
      }
    }
  }

  "Whereas the Maybe monad is for values with an added context of failure, The Wrtier monad must" must {
    "add another value as a log value. This implicit applyLog function adds a log message to a given function application." in new TestContext {
      val a = (3, "Smallish gang. ") applyLog isBigGang
      val e = (false, "Smallish gang. Compared gang size to 9")
      a should be(e)
    }

    "alue as a log value. This implicit applyLog function adds a log message to a given function application." in new TestContext {

      val a = multWithLog run
      val e = (List("Got number: 3", "Got number: 5"), 15)
      a should be(e)
    }

  }
}
