package mike.scalaz.day5

import org.scalatest.{FlatSpec, Matchers}
import scalaz.Scalaz._

class MonadPlusSpec extends FlatSpec with Matchers {

  "MonadPlus" must "support guard function and fromTo(|->) in a for comprehension" in {
    val r = for {
      x <- 1 |-> 50 if x.shows contains '7'
    } yield x
    r should be(List(7, 17, 27, 37, 47))
  }

  "MonadPlus" must "allow  you to append two containers" in {
    val r = List(1,2,3) <+> List(4,5,6)
    r should be (List(1,2,3,4,5,6))
  }

  "MonadPlus" must "support filter function(another kind of guard)" in {
    val r = (1 |-> 50) filter {x => x.shows contains '6'}
    r should be(List(6, 16, 26, 36, 46))
  }
}
