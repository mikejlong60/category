package mike.scalaz.day1

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scala.language.implicitConversions

class YesNoTypeClassSpec extends WordSpecLike with Matchers {

  trait TestContext {

    trait CanTruthy[A] { self =>
      def truthys(a: A): Boolean
    }

    object CanTruthy {
      def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
      def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
        def truthys(a: A): Boolean = f(a)
      }
    }

    trait CanTruthyOps[A] {
      def self: A
      implicit def F: CanTruthy[A]
      final def truthy: Boolean = F.truthys(self)
    }

    object ToCanIsTruthyOps {
      implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
        new CanTruthyOps[A] {
          def self = v
          implicit def F: CanTruthy[A] = ev
        }
    }
  }

  "Truthy" must {
    "succeed for Integer" in new TestContext {
      implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
        case 0 => false
        case _ => true
      })

      import ToCanIsTruthyOps._

      10.truthy should be(true)
      0.truthy should be(false)
    }

    "succeed for String" in new TestContext {
      implicit val strCanTruthy: CanTruthy[String] = CanTruthy.truthys({
        case "no" => false
        case "false" => false
        case "" => false
        case _ => true
      })

      import ToCanIsTruthyOps._

      "hi".truthy should be(true)
      "no".truthy should be(false)
      "false".truthy should be(false)
      "".truthy should be(false)
    }

  }
}