package mike.scalaz.day11

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class LensSpec extends WordSpecLike with Matchers {

  trait TestContext {

    case class Point(x: Double, y: Double)
    case class Color(r: Byte, g: Byte, b: Byte)
    case class Turtle(position: Point, heading: Double, color: Color)

    val turtlePosition = Lens.lensu[Turtle, Point](
      (a, value) => a.copy(position = value), _.position
    )

    val pointX = Lens.lensu[Point, Double](
      (a, value) => a.copy(x = value), _.x
    )

    val turtleX = turtlePosition >=> pointX

  }

  "Lens" must {
    "get the turtle x coordinate using the Kleisli andThen(>=>) operator" in new TestContext {

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

      val e = 2.0

      turtleX.get(t0) should be(e)
    }

    "use the set operator which will always return a new value, not change the original" in new TestContext {

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
      val t1 = turtleX.set(t0, 50.0)

      val newE = 50.0
      val oldE = 2.0

      turtleX.get(t1) should be(newE)
      turtleX.get(t0) should be(oldE)
    }

    "use the mod operator to get the value, apply it to some function and set using the result" in new TestContext {

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
      val t1 = turtleX.mod(x => x + 1.0 , t0)

      val newE = 3.0
      val oldE = 2.0

      turtleX.get(t1) should be(newE)
      turtleX.get(t0) should be(oldE)
    }

    "use the variation to mod that's curried called =>=." in new TestContext {
      val incX = turtleX =>= {_ + 1.0}

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
      val t1 = incX(t0)

      val newE = 3.0
      val oldE = 2.0

      turtleX.get(t1) should be(newE)
      turtleX.get(t0) should be(oldE)
    }

    "the preceding illustrates State transition. It emulates imperative programming " +
      "on top of an immutable data structure. Following is another way to write incX. It " +
      "modifies the portion of the state viewed through the lens and returns its new value." in new TestContext {
      val incX = for {
        x <- turtleX %= {_ + 1.0}
      } yield x


      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
      val t1 = incX(t0)

      val newE = 3.0
      val oldE = 2.0

      turtleX.get(t1._1) should be(newE)
      turtleX.get(t0) should be(oldE)
    }
  }
}
