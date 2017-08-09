package mike.scalaz.day15

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class UnapplySpec extends WordSpecLike with Matchers {
  "Unapply is a way that Scalaz gives you a meta-instance of a typeclass. It" must {
    "not allow you to unapply type Any into a type constructor of kind M[_]." +
      "You cannot allow any type to be promoted as an Applicative. This is good." +
      "No dang Map[String, Any[ allowed!!!" in {
        assertCompiles("implicitly[Unapply[Applicative, Int]]")

        assertDoesNotCompile("implicitly[Unapply[Applicative, Any]]")
      }

    "allow you to rewrite failedTree.sequence with sequenceU, a version that infers the nested type constructor. " +
      "The result of that is less code." in {
      val failedTree = 1.success[String].node(2.success[String].leaf, "boom".failure[Int].leaf)
      val a = failedTree.sequence[({type l[X]=Validation[String, X]})#l, Int]
      val e = failedTree.sequenceU
      a should be (e)
    }
  }
}
