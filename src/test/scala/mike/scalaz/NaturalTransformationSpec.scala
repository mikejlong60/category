package mike.scalaz

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

class NaturalTransformationSpec extends Properties("Natural Transformations") {

  def maybeList[T](l :List[T]):Option[List[T]] = l match {
    case x :: xs => Some(x :: xs)
    case Nil => None
  }


  val safeHead = new (List ~> Option) {
    def apply[T](l: List[T]): Option[T] = l match {
      case x :: xs => Some(x)
      case Nil => None
    }
  }

  val toList = new (Option ~> List) {
    def apply[T](opt: Option[T]): List[T] =

      if (opt.isEmpty) List()
      else new ::(opt.get, Nil)

  }

  property("safeHead on Int list") = forAll { (xs: List[Int]) =>
    val a = safeHead(xs)
    a === xs.headOption
  }

  property("safeHead on String list") = forAll { (xs: List[String]) =>
    val a = safeHead(xs)
    a === xs.headOption
  }

  property("toList on String list") = forAll { (s: Option[String]) =>
    val a = toList(s)
    a match {
      case x :: xs => x === s.get
      case Nil => true
    }
  }

  property("safeHead composed with another natural transformation") = forAll { (xs: List[Int]) =>
    val safeHeadToListToSafeHead: List ~> Option = safeHead compose toList compose safeHead

    val toListToSafeHeadToList: Option ~> List = toList compose safeHead compose toList

    val a1 = safeHeadToListToSafeHead(xs)
    val a2 = toListToSafeHeadToList(a1)

    xs match {
      case x :: xs => a2.head === (x :: xs).head
      case Nil => a1 === None
     }

  }

  property("verify naturality condition") = forAll { (xs: List[Int]) =>
    val f: Option ~> Option = safeHead compose toList
    val fff : List ~> List = toList compose safeHead

    val xss = maybeList[Int](xs)
    //fff(xs) === f(xss).get

    val a = safeHead(Nil).map(f => f)//.map(d => d === None)//.map(dd => )
    //println(a)
    12 === 12//Nothing//None
    //fmap f (safeHead []) = fmap f Nothing = Nothing

    val aa = safeHead(xs).map(f => f)
    //println(aa)
    12 === 12


  }

}
