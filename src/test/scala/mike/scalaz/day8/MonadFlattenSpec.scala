package mike.scalaz.day8

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scalaz._
import Scalaz._
import scala.language.higherKinds

class MonadFlattenSpec extends Properties("Flatten") {

  property("Flatten a nested option") = forAll { (i: Int) =>
    val a = (Some((i).some): Option[Option[Int]]).join
    val e = (i).some
    a == e
  }

  property("Flatten two lists into one") = forAll { (ints: List[Int]) =>
    val (a1, a2) = ints.splitAt(ints.length / 2)
    val a = List(a1, a2).join
    a == ints
  }

  property("Flatten a nested Either right") = forAll { (i: Int) =>
    val a = (i).right[String].right[String].join
    val e = (i).right
    a == e
  }

  property("Flatten a nested Either left") = forAll { (i: String) =>
    val a = (i).left[Int].right[String].join
    val e = (i).left
    a == e
  }
}
