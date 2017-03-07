package mike.scalaz.day4

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scala.language.higherKinds
import scalaz.scalacheck.ScalazProperties.functor
import org.scalacheck.{Gen, Arbitrary}
import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._

class FunctorLawsSpec extends WordSpecLike with Matchers {

  "Identity Law for List functor" must {
    "return the List with no modifications when mapped over identity. " +
      "You get back the same value as the original functor." in {
        List(1, 2, 3) map { identity } should be(List(1, 2, 3))
      }
  }

  "For the Associativity law for List functor it" must {
    "be true that when you compose two functions and then map the resulting " +
      "function over a functor that's the same thing as first mapping one function " +
      "over the functor and then mapping the second function over the functor. " +
      "How you nest them does not matter. " in {
        (List(1, 2, 3) map { { (_: Int) * 3 } map { (_: Int) + 1 } }) should be(List(1, 2, 3) map { (_: Int) * 3 } map { (_: Int) + 1 })
      }
  }

  "A busted functor" must {
    "not follow all the functor laws. " +
      "The following stupid functor breaks the identity law because it does " +
      "not give back same thing you started with when mapped with identity." in {
      sealed trait COption[+A] {}
      case class CSome[A](counter: Int, a: A) extends COption[A]
      case object CNone extends COption[Nothing]

      implicit def coptionEqual[A]: Equal[COption[A]] = Equal.equalA
      implicit val coptionFunctor = new Functor[COption] {
        def map[A, B](fa: COption[A])(f: A => B): COption[B] = fa match {
          case CNone => CNone
          case CSome(c, a) => CSome(c + 1, f(a))
        }
      }

      val hoha = (CSome(0, "ho"): COption[String]) map {(_: String) + "ha"}
      val ho = (CSome(0, "ho"): COption[String]) map {identity}
      ho should not be (CSome(0, "ho"))//This means COption failed the identity law
      implicit def COptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[COption[A]] = a map { a => (CSome(0, a): COption[A]) }

      //functor.laws[COption].check()
    }

  }
}
