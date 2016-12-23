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


  }
}