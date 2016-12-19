package mike

import mike.Monoid._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class MonoidSpec extends Properties("Monoid") {

  property("sum the list of Ints") = forAll { (ints: List[Int]) =>
    sum(ints) == (ints.foldLeft(0)((acc, i) => acc + i))
  }

  property("sum the list of strings") = forAll { (strs: List[String]) =>
    sum(strs) == (strs.foldLeft("")((acc, str) => s"$acc$str"))
  }

  property("make a local multiplication monoid for Int") = forAll { (ints: List[Int]) =>
    val mult: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a * b
      def mzero: Int = 1
    }
    sum(ints)(mult) == (ints.foldLeft(1)((acc, i) => acc * i))
  }
}
