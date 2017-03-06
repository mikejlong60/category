package mike.scalaz.day4

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz._
import Scalaz._
import scala.language.higherKinds

class FunctorLawsSpec extends WordSpecLike with Matchers {

  "Identity Law for List functor" must {
    "return the List with no modifications when mapped over identity. You get back the same value as the original functor." in {
      List(1,2,3) map {identity} should be (List(1,2,3))
    }
  }

  "For the Associativity law for List functor it" must {
    "be true that when you compose two functions and then map the resulting function over a functor that's the same thing as first mapping one function over the functor and then mapping the second function over the functor. How you nest them does not matter. " in {
      (List(1, 2, 3) map {{(_: Int) * 3} map {(_: Int) + 1}}) should be (List(1, 2, 3) map {(_: Int) * 3} map {(_: Int) + 1})
    }
  }
}
