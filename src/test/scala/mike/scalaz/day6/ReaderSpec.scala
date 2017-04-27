package mike.scalaz.day6

import org.scalatest.{Matchers, WordSpecLike}

import scalaz.Scalaz._
import scalaz.{Monoid, Writer}

class ReaderSpec extends WordSpecLike with Matchers {

  trait TestContext {
    val f = (_: Int) * 5

    val g = ({ (_: Int) * 2 } |@| { (_: Int) + 10 }) { _ + _ }

    val addStuff: Int => Int = ((_: Int) * 2).flatMap(a => ((_: Int) + 10).map(b => a + b))

  }

  "Functions are applicative functors, allowing one to operate on eventual results as if they already existed." must {
    "g applied to 3 is 19." in new TestContext {
      val a = g(3)
      val e = 19
      a should be(e)
    }
  }

  "Functions are also monads. Just like other monads, a functor is a value with context. That context is that the value is" +
    " not present yet and you have to apply the function to something to get its value." must {
      "in addStuff both *2 and +10 get applied to the number 3. return (a+b) does as well, but it ignores it and always" +
        "presents a+b as the result.  For this reason the function monad is also called the reader monad because" +
        "all the functions read from a common source.  Essentially, reader lets you pretend that the value" +
        "is already there." in new TestContext {
          val a = addStuff(3)
          val e = 19
          a should be(e)
        }
    }

}
