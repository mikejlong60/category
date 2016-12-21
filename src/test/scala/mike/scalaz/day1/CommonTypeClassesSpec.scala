package mike.scalaz.day1

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

import scalaz.Scalaz._
import scalaz._

class CommonTypeClassesSpec extends Properties("Equality, Ord, Show, Enum, Bounded type classes") {

  property("Int equality") = forAll { (i: Int) =>
    i === i
  }

  property("String equality") = forAll { (i: String) =>
    i === i
  }

  property("Int inequality") = forAll { (i: Int) =>
    i =/= i + 1
  }

  property("String inequality") = forAll { (i: String) =>
    i =/= s"-$i"
  }

  property("Int ordering") = forAll { (i: Int) =>
    val bigger = i - 100
    val smaller = i - 101
    bigger gt smaller
  }

  property("String ordering") = forAll { (i: String) => (i.size > 3) ==> (i gt i.substring(2, 2)) }

  property("Show Int") = forAll { (i: Int) => i.show === s"$i" }

  property("Enum Int") = forAll { (_: Int) =>
    val r = (100 |=> 2000).toList
    r.foldLeft(false)((_, x) => (x.succ gt x) && (x.pred lt x))
  }

  property("Bounded Long") = forAll { (x:Int) =>
    val max = implicitly[Enum[Long]].max
    max.get gt x
  }

  property("Bounded Int") = forAll { (x:Short) =>
    val max = implicitly[Enum[Int]].max
    max.get gt x
  }

}
