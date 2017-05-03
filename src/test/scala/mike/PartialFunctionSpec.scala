package mike

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class PartialFunctionSpec extends Properties("PartialFunctions") {

  property("use orElse to apply a list of partial functions to a list of Options") = forAll { (i: List[Option[Int]]) =>
    val maybeNoDivByZero: PartialFunction[Option[Int], Int] = { case Some(i) if i != 0 ⇒ 1/i }

    val maybeMultiply: PartialFunction[Option[Int], Int] = { case Some(i) ⇒ 8888 }

    val cya: PartialFunction[Option[Int], Int] = { case None ⇒ 9999 }

    val r = i map (maybeNoDivByZero orElse maybeMultiply orElse cya)

    r.size == i.size
  }


  property("make a transformation pipeline that uses orElse on Partial functions follows by andThen to append some other functions to a list of Options") = forAll { (i: List[Option[Int]]) =>
    val maybeNoDivByZero: PartialFunction[Option[Int], Int] = { case Some(i) if i != 0 ⇒ 1/i }

    val maybeMultiply: PartialFunction[Option[Int], Int] = { case Some(i) ⇒ 8888 }

    val cya: PartialFunction[Option[Int], Int] = { case None ⇒ 9999 }

    def add12(i: Int): String = s"$i"

    def makeFloat(i: String): Float = i.toInt / 12.5f

    val add2: Float => Int = i => i.toInt + 2

    val makeZero: Int => Int = i => 0

    val something = (maybeNoDivByZero orElse  maybeMultiply orElse cya)
    val r = i map {something andThen add12 andThen makeFloat andThen add2 andThen makeZero}

    r.size == i.size



//    val r2 = i map {add12 compose makeFloat compose add2 commpose (maybeNoDivByZero orElse  maybeMultiply orElse cya) }


  }

}
