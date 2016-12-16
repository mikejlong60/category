package mike

import mike.Category._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


class CategorySpec extends Properties("Category") {

  val int_to_str = (i: Int) => i.toString
  val str_len = (s: String) => s.length
  val square = (i: Int) => i * i

  property("satisfy associativity") = forAll { (i: Int) =>
    compose(square, compose(str_len, int_to_str))(i) == (compose(compose(square, str_len), int_to_str)(i))
  }

  property("satisfy identity") = forAll { (i: Int) =>
    compose(int_to_str, id[Int])(i) == (compose(id[String], int_to_str)(i))
  }
}
