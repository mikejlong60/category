package mike.scalaz.day15

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._
//import Id._

class ParallelCompositionSpec extends WordSpecLike with Matchers {

  trait TestContext {
    val f = { (x: Int) => x + 1 }
    val g = { (x: Int) => List(x, 5) }
    val h = f &&& g
  }

  "Understand what's not so great about traverseU. It" must {
    "traverseU f is fine" in new TestContext {
      val a = List(1, 2, 3) traverseU f

      a should be(9)

    }
    "traverseG f is fine" in new TestContext {
      val a = List(1, 2, 3) traverseU g

      val e = List(List(1, 2, 3), List(1, 2, 5), List(1, 5, 3), List(1, 5, 5), List(5, 2, 3), List(5, 2, 5), List(5, 5, 3), List(5, 5, 5))

      a should be(e)

    }
    "traverseU h is goofy. It should work like (f(x), g(x)" in new TestContext {
      val a = List(1, 2, 3) traverseU h
      val e = List(1, 2, 3) traverseU { (x: Int) => (f(x), g(x)) }

      a should be(e)
      e should be((9, List(List(1, 5), List(2, 5), List(3, 5))))
    }

    "traverse more" in new TestContext {
      val text = "the cat in the hat\n sat on the mat\n".toList
      def count[A] = (a: A) => 1
      val charCount = count[Char]
      val a = text traverseU charCount
      a should be (35)

      import scalaz.std.boolean.test

      val lineCount = (c: Char) => test(c.equals('\n'))
      val a2 = text traverseU lineCount
      a2 should be (2)

      val wordCount = (c: Char) => for {
        x <- get[Boolean]
        val y = c =/= ' '
        _ <- put(y)
      } yield test(y /\ !x)

      val a3 = (text traverseU wordCount) eval false count(_ > 0)
      a3 should be (9)

      val a4 = text traverseU {(c: Char) => (charCount(c), lineCount(c))}
      a4 should be (35, List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
    }
  }
}
