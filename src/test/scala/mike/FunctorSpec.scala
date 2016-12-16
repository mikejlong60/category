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
        fmap(stringID)(os) == optionID(os)
    }

  property("preserve composition for OptionFunctor") =
    forAll {
      import mike.Functor.OptionFunctor._
      val f = (i: Int) => i.toString
      val g = (s: String) => s.length
      (maybe: Option[Int]) =>
        fmap(g compose f)(maybe) == (fmap(g) compose fmap(f))(maybe)
    }

  property("run your dang Functor0 functor") =
    forAll {
      (s: String) =>
        import mike.Functor.Function0Functor._
        val f = (s2: String) => s2.length
        val lifted = fmap(() => s)(f)
        lifted() == s.length
    }

  property("preserve identity for Function0 Functor") =
    forAll {
      (s: String) =>
        import mike.Functor.Function0Functor._
        val f = (s2: String) => s2.length
        val lifted = fmap(() => s)(f)
        lifted() == f(s)
    }
}