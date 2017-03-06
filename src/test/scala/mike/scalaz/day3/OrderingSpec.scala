package mike.scalaz.day3

import org.scalatest.{Matchers, WordSpecLike}

import scalaz.Tag
import scalaz.Ordering
import scalaz._
import Scalaz._
import scala.language.higherKinds
import scalaz.Ordering.GT

class OrderingSpec extends WordSpecLike with Matchers {


  def lengthCompare(lhs: String, rhs: String): Ordering = (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)

  "Ordering" must {
    "allow you to write a comparison function that compares length and if the length is the same it compares order" in {
      lengthCompare("zzz", "aaaa") should be (Ordering.LT)
      lengthCompare("zzzz", "aaaa") should be (Ordering.GT)
    }
  }
}
