package mike.scalaz.day2

import org.scalatest.{Matchers, WordSpecLike}

import scalaz.Scalaz._
import scalaz._
import scalaz.syntax.Ops

class ApplicativeFunctorSpec extends WordSpecLike with Matchers {

  "ScalaZ Applicative Functors" must {
    "Regular Scala map does not take functions with more than one parameter without currying" in  {
      val f = List(1, 2, 3, 4) map {(x: Int, y: Int) => x * y}.curried
      (f map(_(9))) should be (List(9, 18, 27, 36))
    }

    "In ScalaZ Point and Pure are the same thing" in  {
      val x =1.point[List]
      val y = List(1)
      x should be (y)
      1.pure[List] should be (y)

      (1.point[Option] map (_ + 2)) should be (Some(3))
    }

    "Apply or  <*> is a beefed-up fmap except that fmap takes a fuction and a functor and applies the function " +
      "inside the functor value, <*> " +
      "takes a functor that has a function in it and another functor and extracts that function from the first " +
      "functor and then maps it over the second one." in {
      (9.some <*> {(x: Int) => x + 3}.some) should be (Some(12))
    }

    "The left-hand-side function returns the right value" in {
      (1.some <* 2.some) should be (Some(1))
    }

    "The right-hand-side function returns the left value" in {
      (1.some *> 2.some) should be (Some(2))
    }

    "If either one is None then the right or left side functions return None" in {
      (none <* 2.some) should be (none)

      (1.some *> none) should be (none)
    }

    "You can run map over functions that take more than one parameter without using the 'curried' thing" in {
      val noCurry1 = 9.some <*> {(x: Int) => x + 3}.some <*> {(x: Int) => x * 10}.some
      noCurry1 should be (Some(120))

      val noCurry2 = 9.some <*> {(x: Int) => x + 3}.some
      val curried= 9.some <*> {3.some <*> {(x:Int, y: Int) => x + y}.curried.some}
      curried should be (noCurry2)
    }

    "You can extract values from containers and apply them to a single function" in {
      (^(3.some, 12.some) {_ + _}) should be (Some(15))
    }

    "As always, when one side fails the whole expression fails" in {
      (^(3.some, none[Int]) {_ + _}) should be (none)
    }

    "You can also use the |@| function to do the same thing as above. The one above can't take two type " +
      "parameters" in {
      ((3.some |@| none[Int]) {_ + _}) should be (none)
      ((3.some |@| 9.some) {_ + _}) should be (12.some)
    }

    "Lists as Apply" must {
      "the [] Type constructor for Lists is an applicative functor. I am starting to understand type constructors in Scala. " +
        "They are part of the type-parameter thing" in {
        (List(1,2,3) <*> List((_:Int) * 0, (_:Int) + 100, (x:Int) => x * x)) should be (List(0,0,0,101,102,103,1,4,9))
      }
    }
  }
}