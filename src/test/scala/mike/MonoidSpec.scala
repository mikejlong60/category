package mike

import mike.Monoid._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class MonoidSpec extends Properties("Monoid")  {

  property("sum the list of Ints") = forAll { (ints: List[Int]) =>
    sum(ints) == (ints.foldLeft(0)((acc, id) => acc + id))
  }

  property("sum the list of strings") = forAll { (strs: List[String]) =>
    sum(strs) ==  (strs.foldLeft("")((acc, str) => s"$acc$str"))
  }
}
