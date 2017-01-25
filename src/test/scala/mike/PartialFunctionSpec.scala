package mike

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class PartialFunctionSpec extends Properties("PartialFunctions") {

  property("apply a list of partial functions to a list of Options") = forAll { (i: List[Option[Int]]) =>
    println(s"i: $i")

    val maybeNoDivByZero: PartialFunction[Option[Int], Int] = { case Some(i) if i != 0 ⇒ 1/i }

    val maybeMultiply: PartialFunction[Option[Int], Int] = { case Some(i) ⇒ 8888 }

    val cya: PartialFunction[Option[Int], Int] = { case None ⇒ 9999 }

    val r = i map (maybeNoDivByZero orElse maybeMultiply orElse cya)
    println(s"i: ${i}")
    println(s"r: ${r}")

    r.size == i.size
  }


}
