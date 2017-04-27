package mike.scalaz.day7

import org.scalatest.{Matchers, WordSpecLike}

import scalaz.Scalaz._
import scalaz.State

class StateMonadSpec extends WordSpecLike with Matchers {

  trait NoStateTestContext {
    type Stack = List[Int]

    def pop(stack: Stack): (Int, Stack) = stack match {
      case x :: xs => (x, xs)
    }

    def push(a: Int, stack: Stack): (Unit, Stack) = ((), a :: stack)

    def stackManip(stack: Stack): (Int, Stack) = {
      val (_, newStack1) = push(3, stack)
      val (a, newStack2) = pop(newStack1)
      pop(newStack2)
    }
  }

  trait StateTestContext {
    type Stack = List[Int]

    val pop = State[Stack, Int] {
      case x :: xs => (xs, x)
    }

    def push(a: Int) = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }

    def stackManip: State[Stack, Int] = for {
      _ <- push(3)
      a <- pop
      b <- pop
    } yield (b)

    def stackManipNoSugar: State[Stack, Int] = push(3).flatMap {
      case _ => pop.flatMap(a => pop.map(b => b + 12))

    }
  }

  "First we must" must {
    "demonstrate why we need it." in new NoStateTestContext {
      val a = stackManip(List(5, 8, 2, 1))
      val e = (5, List(8, 2, 1))
      a should be(e)
    }
  }

  "Then we must" must {
    "demonstrate that you can chain operations in a for comprehension without having to pass around the Stack " +
      "values like you did without the use of the State monad " in new StateTestContext {
        val a = stackManip(List(5, 8, 2, 1))
        val e = (List(8, 2, 1), 5)
        a should be(e)
      }

    "demonstrate that you can chain operations with map and flatMap " +
      "without having to pass around the Stack values like you did " +
      "without the use of the State monad " in new StateTestContext {

        val a = stackManipNoSugar(List(5, 8, 2, 1))
        val e = (List(8, 2, 1), 17)
        a should be(e)
      }
  }

}
