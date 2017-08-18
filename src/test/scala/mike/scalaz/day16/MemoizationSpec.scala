package mike.scalaz.day16

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class MemoizationSpec extends WordSpecLike with Matchers {

  trait TestContext {
    val slowFib: Int => Int = {
      case 0 => 0
      case 1 => 1
      case n => slowFib(n - 2) + slowFib(n - 1)
    }

    val fastFib: Int => Int = Memo.mutableHashMapMemo {
      case 0 => 0
      case 1 => 1
      case n => fastFib(n - 2) + fastFib(n - 1)
    }
  }

  "Understand how to use memoization" must {
    "run without memoization" in new TestContext {
      val a = slowFib(30)
      a should be(832040)
    }

    "run with memoization" in new TestContext {
      val s1 = System.currentTimeMillis()
      val a1 = slowFib(30)
      val et1 = System.currentTimeMillis() - s1
      println(s"slow took[${System.currentTimeMillis() - s1} ms]")
      a1 should be(832040)
      val s2 = System.currentTimeMillis()
      val a2 = fastFib(30)
      val et2 = System.currentTimeMillis() - s2
      println(s"fast took[${System.currentTimeMillis() - s2} ms]")
      a2 should be(832040)
      et2 should be < (et1)
    }

  }
}
