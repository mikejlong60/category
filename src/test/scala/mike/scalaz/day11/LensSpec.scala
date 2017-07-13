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
    "let you move the Turtle around a space with immutable state" in new TestContext {

      val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

      val e = 2.0

      turtleX.get(t0) should be(e)
    }
  }
}
