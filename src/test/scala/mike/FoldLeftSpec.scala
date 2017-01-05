package mike

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import mike.FoldLeft._
import mike.Monoid._
import scala.language.higherKinds

class FoldLeftSpec extends Properties("FoldLeft") {

  def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
    val m = implicitly[Monoid[A]]
    val fl = implicitly[FoldLeft[M]]
    fl.foldLeft(xs, m.mzero, m.mappend)
  }

  property("sum the list of Ints") = forAll { (ints: List[Int]) =>
    sum(ints) == (ints.foldLeft(0)((acc, i) => acc + i))
  }

  property("sum the list of strings") = forAll { (strs: List[String]) =>
    sum(strs) == (strs.foldLeft("")((acc, str) => s"$acc$str"))
  }
}
