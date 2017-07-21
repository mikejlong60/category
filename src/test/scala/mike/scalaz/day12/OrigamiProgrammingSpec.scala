package mike.scalaz.day12

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz._
import Scalaz._

class OrigamiProgrammingSpec extends WordSpecLike with Matchers {

  "When unfolding you" must {
    "be able to unfold a difference list. A difference list has constant-time appending" in {
      val a = DList.unfoldr(10, {(x: Int) => if (x ==0) none else (x, x - 1).some})
      val e = DList(10, 9,8,7,6,5,4,3,2,1).toList
      a.toList should be (e)
    }

    "be able to unfold to a stream" in {
      val a = unfold(10) {(x) => if (x ==0) none else (x, x - 1).some}
      val e = Stream(10, 9,8,7,6,5,4,3,2,1)
      a should be (e)
    }

    "show a more complicated example of a selection sort that uses unfold" in {
      def minimumS[A: Order](stream: Stream[A]) = stream match {
        case x #:: xs => xs.foldLeft(x) {_ min _} //#:: pattern matches on a stream with a head and a tail
      }

      def deleteS[A: Equal](y: A, stream: Stream[A]): Stream[A] = (y, stream) match {
        case (_, Stream()) => Stream()
        case (y, x #:: xs) =>
          if (y == x) xs
          else x #:: deleteS(y, xs)
      }

      def delim[A: Order](stream: Stream[A]): Option[(A, Stream[A])] = stream match {
        case Stream() => none
        case xs =>
          val y = minimumS(xs)
          (y, deleteS(y, xs)).some
      }

      def ssort[A: Order](stream: Stream[A]): Stream[A] = unfold(stream){delim[A]}

      val a = ssort(Stream(1,2,3,4,5,1,0,-12,123)).toList
      val e = List(-12, 0, 1, 1, 2, 3, 4, 5, 123)
      a should be (e)
    }
  }
}
