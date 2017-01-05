package mike

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scala.language.higherKinds
import scala.language.implicitConversions

class MoniodOpSpec extends Properties("MoniodOp") {

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  property("sum the list of Ints") = forAll { (x: Int, y: Int) => (x |+| y) == (x + y)}
  property("sum the list of Strings") = forAll { (x: String, y: String) => (x |+| y) == (s"$x$y")}
}
