package mike.scalaz.day17

import org.scalatest.{Matchers, WordSpecLike}

import scalaz._
import Scalaz._
import effect._
import IO._

class IOMonadSpec extends WordSpecLike with Matchers {
  trait TestContext {
    val action1 = IO {
      val source = scala.io.Source.fromFile("./README.md")
      source.getLines.toStream
    }

    val fileContents = "  # Experiments on Categories and Functors from https://hseeberger.wordpress.com/2010/11/25/introduction-to-category-theory-in-scala/"
  }

  "Read an input file and print it out with obvious non-safety disclaimer" in new TestContext {
    val a = action1.unsafePerformIO().toList
    a should be (List(fileContents))
  }

  "Compose IO monads with obvious non-safety disclaimer" in new TestContext {
    val a = (action1 |+| action1).unsafePerformIO().toList

    a should be (List(fileContents, fileContents))
  }

}
