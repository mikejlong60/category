package mike

import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, Test}

class FunctorSpec extends Properties("List Functor") {

  property("preserve identity for ListFunctor") =
    forAll {
      import mike.Functor.ListFunctor._
      val stringID = (s: String) => s
      val stringListID = (ss: List[String]) => ss
      (ss: List[String]) =>
        //println(s"string list: $ss")
        fmap(stringID)(ss) == stringListID(ss)
    }

  property("preserve composition for ListFunctor") =
    forAll {
      import mike.Functor.ListFunctor._
      val f = (i: Int) => i.toString
      val g = (s: String) => s.length
      (is: List[Int]) =>
        fmap(g compose f)(is) == (fmap(g) compose fmap(f))(is)
    }

  property("preserve identity for OptionFunctor") =
    forAll {
      import mike.Functor.OptionFunctor._
      val stringID = (s: String) => s
      val optionID = (ss: Option[String]) => ss
      (os: Option[String]) =>
        //println(s"option string: $os")
        fmap(stringID)(os) == optionID(os)
    }

  property("preserve composition for OptionFunctor") =
    forAll {
      import mike.Functor.OptionFunctor._
      val f = (i: Int) => i.toString
      val g = (s: String) => s.length
      (maybe: Option[Int]) =>
        //println(s"maybe option[Int]: $maybe")
        fmap(g compose f)(maybe) == (fmap(g) compose fmap(f))(maybe)
    }

  property("run your dang functor0 functor") =
    forAll {
      (s: String) =>
        runFunctor0(s)
    }

  def runFunctor0(s: String) = {
    import mike.Functor.Function0Functor._
    val f = (s: String) => s.length
    val lifted = fmap(() => s)(f)
    println("lifted:" + lifted())
    lifted() == s.length
  }

  //  property("preserve composition for Function0Functor") = forAll {
  //    import mike.Functor.Function0Functor._
  //    val f = (s: String) => s.length
  //    val lifted = fmap(() => "abcd")(f) //(s: String) => s.length
  //    (s: String) =>
  //      //println(s"function0 : $s")
  //      lifted() == 3
  //    //fmap(lifted compose f)(maybe) == (fmap(lifted) compose fmap(f))(maybe)
  //  }

}