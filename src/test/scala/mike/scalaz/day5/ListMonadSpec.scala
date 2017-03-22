package mike.scalaz.day5

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class ListMonadSpec extends WordSpecLike with Matchers {

  "List Monad" must {
    "support Applicatives with ^" in {
      val r = ^(List(1, 2, 3), List(10, 100, 100)) { _ * _ }
      r should be(List(10, 100, 100, 20, 200, 200, 30, 300, 300))
    }

    "support Applicatives with >>=" in {
      val r = List(1, 2, 3) >>= { x => List(x, -x) }
      r should be(List(1, -1, 2, -2, 3, -3))
    }

    "support Applicatives with for notation" in {
      val r = for {
        n <- List(1, 2)
        ch <- List('a', 'b')
      } yield (n, ch)
      println(r)
      r should be(List((1,'a'), (1, 'b'), (2, 'a'), (2, 'b')))
    }
  }
}
