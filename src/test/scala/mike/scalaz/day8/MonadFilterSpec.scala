package mike.scalaz.day8

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class MonadFilterSpec extends Properties("Filter") {

  property("Filter out nothing, just make every possiblity") = forAll { (i1: Int, i2: Int) =>
    val a = List(i1,i2) filterM (x => List(true, true))
    a == List(List(i1,i2),List(i1,i2),List(i1,i2),List(i1,i2))
  }

  property("Filter out everything, and make every possiblity") = forAll { (i1: Int, i2: Int) =>
    val a = List(i1,i2) filterM (x => List(false, false))
    a == List(List(),List(),List(),List())
  }

}
