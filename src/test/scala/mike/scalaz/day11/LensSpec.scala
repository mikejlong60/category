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
      val t1 = turtleX.mod(x => x + 1.0, t0)

      val newE = 3.0
      val oldE = 2.0

      turtleX.get(t1) should be(newE)
      turtleX.get(t0) should be(oldE)
    }

    "use the variation to mod that's curried called =>=." in new TestContext {
      val incX = turtleX =>= { _ + 1.0 }

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
          x <- turtleX %= { _ + 1.0 }
        } yield x

        val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
        val t1 = incX(t0)

        val newE = 3.0
        val oldE = 2.0

        turtleX.get(t1._1) should be(newE)
        turtleX.get(t0) should be(oldE)
      }

    "move turtle forward without using any inconvenient copy operations deep in the object tree" in new TestContext {

      val turtleHeading = Lens.lensu[Turtle, Double]((a, value) => a.copy(heading = value), _.heading)

      val pointY = Lens.lensu[Point, Double]((a, value) => a.copy(y = value), _.y)

      val turtleY = turtlePosition >=> pointY

      def forward(dist: Double) = for {
        heading <- turtleHeading
        x <- turtleX += dist * math.cos(heading)
        y <- turtleY += dist * math.cos(heading)
      } yield (x, y)

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
      val a = forward(10.0)(t0)

      val e = forward(10.0) exec (t0)

      a._1 should be(e)
    }
  }

  "Lens Laws" must {
    "get the same value back every time from a repeated get" in new TestContext {
      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

      val a1 = turtleX.get(t0)
      val a2 = turtleX.get(t0)
      val a3 = turtleX.get(t0)
      a1 should be(a2)
      a2 should be(a3)
    }

    "after issuing get, if you set the same value that you got nothing changes" in new TestContext {

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

      val a1 = turtleX.get(t0)
      val a2 = turtleX.set(t0, t0.position.x)
      val a3 = turtleX.set(t0, t0.position.x)

      a2 should be(t0)
      a3 should be(t0)
    }

    "if you set twice you get back the second set" in new TestContext {
      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

      val a2 = turtleX.set(t0, 5)
      val a3 = turtleX.set(t0, 10)

      a3 should be (Turtle(Point(10.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte)))

      val a4 = turtleX.set(a3, 50)
      a3 should be (Turtle(Point(10.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte)))
      t0 should be (Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte)))
    }
  }
}
