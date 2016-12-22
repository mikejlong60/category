package mike.scalaz.day2

import org.scalatest.{Matchers, WordSpecLike}
import scalaz.syntax.Ops
import scalaz.Scalaz._
import scalaz._

class FunctorSpec extends WordSpecLike with Matchers {

  trait TestContext {

    trait Functor[F[_]] { self =>
      //Lift `f` into `F` and apply to `F[A]]`
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait FunctorOps[F[_], A] extends Ops[F[A]] {
      implicit def F: Functor[F]

      import Leibniz.===

      final def map[B](f: A => B): F[B] = F.map(self)(f)
    }

  }

  "Functor" must {
    "update only the last element for tuples" in new TestContext {
      ((1, 2, 3) map { _ + 1 }) should be((1, 2, 4))
    }

    "allow for Functions to be functors" in new TestContext {
      ((((x: Int) => x + 1) map (_ * 10))(3)) should be(40)
    }

    "lift a function to a list of ints" in new TestContext {
      val fu = Functor[List].lift {(_: Int) * 3}
      (fu(List(6, 7))) should be (List(18, 21))
    }

    "lift a function to a Some Option." in new TestContext {
      val fu = Functor[Option].lift {(_: Int) * 3}
      (fu(Some(6))) should be (Some(18))
    }

    "lift a function to a None Option." in new TestContext {
      val fu = Functor[Option].lift {(_: Int) * 3}
      (fu(None)) should be (None)
    }

    "lift a function to a list of Strings" in new TestContext {
      val fu = Functor[List].lift {(_: String) + " Dude!"}
      (fu(List("Hi", "Yo"))) should be (List("Hi Dude!", "Yo Dude!"))
    }
  }
}