package mike.scalaz.day5

import org.scalatest.{FlatSpec, Matchers}

import scalaz.Monad
import scalaz.Scalaz._

class MonadLawsSpec extends FlatSpec with Matchers {

  trait TestContext {
    type Birds = Int

    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] = if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none

      def landRight(n: Birds): Option[Pole] = if (math.abs((right + n) - left) < 4) copy(right = right + n).some
      else none

      def banana: Option[Pole] = none

    }
  }

  "Left Identity Law says that when you take a value, " +
    "put it in a default context with return(or point in this case), " +
    "and then feed it to a function by using >>=(same as flatMap), the result" must
    "be the same as taking the value and applying the function to it." in {
      (Monad[Option].point(3) >>= { x => (x + 10000).some }) should be(3 |> { x => (x + 10000).some })
    }

  "Right Identity Law says that when you take a monadic value " +
    "and use >>=(same as flatMap) to feed it to return, the result" must
    "be the same as taking the value and applying the function to it." in {
      ("move on up".some >>= { Monad[Option].point(_) }) should be("move on up".some)
    }

  "Associativity law states that when you have a chain of monadic function" +
    "applications with >>=(flatMap) it" must "not matter how they are nested" in new TestContext {
      val l = Monad[Option].point(Pole(0, 0)) >>= { _.landRight(2) } >>= { _.landLeft(2) } >>= { _.landRight(2) }
      val r = Monad[Option].point(Pole(0, 0)) >>=
        { x =>
          x.landLeft(2) >>= {
            y =>
              y.landRight(2) >>= {
                z => z.landRight(2)
              }
          }
        }
      l should be(r)
    }

}
