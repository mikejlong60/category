package mike.scalaz.day8

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.language.higherKinds
import scalaz.{Kleisli, Reader}
import scalaz.Scalaz._

class MonadComposingSpec extends Properties("FoldLeftM") {

  property("Compose a Kleisli") = forAll { (i1: Int) =>
    val f = Kleisli {(x: Int) => (x + 1).some}
    val g = Kleisli {(x: Int) => (x * 100).some}

    val a = i1.some >>= (f <==< g)

    val e = (i1 * 100) +  1

    //f <==< g is the same as f compose g. g gets derived first and is then shoveled to f
    // f >==> g is the same as f andThen g. f gets derived first and shoveled to g
    a == e.some
  }

  property("AndThen a Kleisli") = forAll { (i1: Int) =>
    val f = Kleisli {(x: Int) => (x + 1).some}
    val g = Kleisli {(x: Int) => (x * 100).some}

    val a = i1.some >>= (f >==> g)

    val e = (i1 + 1) * 100

    //f <==< g is the same as f compose g. g gets derived first and is then shoveled to f
    // f >==> g is the same as f andThen g. f gets derived first and shoveled to g
    a == e.some
  }

  property("rewrite Reader from day 6 as a Kleisli. Reader is an alias for Scalaz Kliesli") = forAll { (i1: Int) =>
    val addStuff: Reader[Int, Int] = for {
      a <- Reader { (x:Int) => x * 2}
      b <- Reader { (x:Int) => x + 10}
    } yield a + b
    val a = addStuff(i1)
    a == (i1 * 2) + (i1 + 10)
  }


}
