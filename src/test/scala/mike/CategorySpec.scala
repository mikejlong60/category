package mike

import mike.Category._
import org.scalacheck.Prop.{BooleanOperators, forAll}
//import org.scalacheck.Prop._
import org.scalacheck.Properties


class CategorySpec extends Properties("Category") {

  val int_to_str = (i: Int) => i.toString
  val str_len = (s: String) => s.length
  val square = (i: Int) => i * i

  property("satisfy associativity") = forAll { (i: Int) =>
    println("satisfy associativity")
    println(s"i=$i")
    println(s"compose(str_int, int_str)(i)=${compose(str_len, int_to_str)(i)}")
    println(s"compose(int_int, compose(str_int, int_str))(i)=${compose(square, compose(str_len, int_to_str))(i)}")
    println(s"(compose(compose(int_int, str_int), int_str)(i))=${(compose(compose(square, str_len), int_to_str)(i))}")
    println("================")
    compose(square, compose(str_len, int_to_str))(i) == (compose(compose(square, str_len), int_to_str)(i))
  }

  property("satisfy identity") = forAll { (i: Int) =>
    println("satisfy identity")
    println(s"i:$i")
    println(s"id[Int]:${id[Int](i)}")
    println(s"compose(int_str, id[Int])(i):${compose(int_to_str, id[Int])(i)}")
    println(s"(compose(id[String], int_str)(i):${(compose(id[String], int_to_str)(i))}")
    println("================")
    compose(int_to_str, id[Int])(i) == (compose(id[String], int_to_str)(i))
  }
}
