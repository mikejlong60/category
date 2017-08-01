package mike.scalaz.day6

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.postfixOps
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

    def gcd(a: Int, b: Int): Writer[Vector[String], Int] =
      if (b == 0) for {
        _ <- Vector("Finished with " + a.shows).tell
      } yield a
      else
        Vector(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell >>= { _ => gcd(b, a % b) }

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

    "multWithLog should add a log to a function application." in new TestContext {

      val a = multWithLog run
      val e = (List("Got number: 3", "Got number: 5"), 15)
      a should be(e)
    }

    "gcd should add a log to the gcd function application" in new TestContext {
      val a = gcd(843, 12) run
      val e = (List("843 mod 12 = 3", "12 mod 3 = 0", "Finished with 3"), 3)
      a should be(e)
    }

  }
}
