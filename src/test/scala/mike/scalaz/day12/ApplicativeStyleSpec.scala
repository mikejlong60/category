package mike.scalaz.day12

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class ApplicativeStyleSpec extends WordSpecLike with Matchers {

  "When using Monoidal Applicatives you" must {
    "be able to turn an Int monoid into an applicative" in {
      val a = Monoid[Int].applicative.ap3(1, -2, 4)(10)
      val e = 13
      a should be(e)
    }

    "be able to turn a List monoid into an applicative" in {
      val a = Monoid[List[Int]].applicative.ap3(List(1), List(-2), List(4))(List(12))
      val e = List(12, 1, -2, 4)
      a should be(e)
    }
  }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we make a product of List and Option" in {
      val a = Applicative[List].product[Option].point(1)
      val e = (List(1), Some(1))
      a should be(e)
    }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we append two tuples of (List, Option)" in {
      val a = ((List(1), 1.some) |@| (List(2), 2.some)) {
        _ |+| _
      }
      val e = (List(1, 2), Some(3))
      a should be(e)
    }

  "When combining Applicative Functors you can fuse idiomatic effects into one, their product. " +
    "Here we append two tuples of (List, try), one try being a failure" in {
      val a = ((List(1), 1.success[String]) |@| (List(2), "boom".failure[Int])) {
        _ |+| _
      }
      val e = (List(1, 2), Failure("boom"))
      a should be(e)
    }

  "Unlike monads in general, applicative functors are closed under composition. Two" +
    "sequentially independent effects can be fused into one, their composition." +
    "Here we compose List and Option" in {
      val a = Applicative[List].compose[Option].point(10)
      val e = (List(10.some))
      a should be(e)
    }

  "The ⊗ and ⊙ operators allow you to combine applicative-style computations in" +
    "two different ways: parallel and sequential. Here we use traverse for a list, " +
    "the same as parallel in ScalaZ.  " must {
      "If the condition is not met the whole expression returns " +
        "none.  Traverse combines flatMap and sequence I think." in {

          val a = List(1, 2, 3) traverse { x => (x > 2) option (x + 1) }
          a should be(none)
        }

      "If the condition is met the expression returns a new list incrememented by the " +
        "function. Traverse combines flatMap and sequence I think." in {

          val a = List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }
          val e = Some(List(2, 3, 4))
          a should be(e)
        }

      "For a monoidal applicative functor, traversal accumulates values." in {
        val a = Monoid[Int].applicative.traverse(List(1, 2, 3)) { x => x + 1 }
        val e = 9
        a should be(e)
      }
    }

  "Shape and contents" must {
    "be paramametrically polymorphic in the collection elements as well as being parameterized" +
      "along two other dimensions: the datatype being traversed, and the applicative functor" +
      "in which the traversal is interpreted. Specializing the applicative functor as a monoid" +
      "yields a generic contents operation." in {

        def contents[F[_]: Traverse, A](f: F[A]): List[A] = Monoid[List[A]].applicative.traverse(f) { List(_) }

        val a = contents(List(1, 2, 3, 4, 5))
        a should be(List(1, 2, 3, 4, 5))

        val a2 = contents(List(Some(1), Some(2), Some(3), none))
        a2 should be(List(Some(1), Some(2), Some(3), none))

        val a3 = contents('P'.node('O'.leaf, 'L'.leaf))
        a3 should be(List('P','O','L'))

      }

      "Take any data structure that supports the traverse method and turn" +
        " it into a list and rewrite contents using that Traverse" in {

        def contents[F[_]: Traverse, A](f: F[A]): List[A] =  f.traverse[({type l[X] = List[A]})#l, A] {List(_)}

        val a = contents(List(1, 2, 3, 4, 5))
        a should be(List(1, 2, 3, 4, 5))

        val a2 = contents(List(Some(1), Some(2), Some(3), none))
        a2 should be(List(Some(1), Some(2), Some(3), none))

        val a3 = contents('P'.node('O'.leaf, 'L'.leaf))
        a3 should be(List('P','O','L'))

      }

      "The `identity idiom` is the Id monad in Scalaz" in {
        def shape[F[_]: Traverse, A](f: F[A]): F[Unit] = f traverse (x => ((): Id[Unit]))

        val a = shape(List(1,2,3))
        a should be (List((),(),()))

        val tree = 'P'.node('O'.leaf, 'L'.leaf)
        println(shape(tree).drawTree)

      }


    "The pair of traversals illustrates the two aspects of iteration that are important:" +
      " mapping and accumulation" in {

      def shape[F[_]: Traverse, A](f: F[A]): F[Unit] = f traverse (x => ((): Id[Unit]))

      def contents[F[_]: Traverse, A](f: F[A]): List[A] =  f.traverse[({type l[X] = List[A]})#l, A] {List(_)}

      def decompose[F[_]: Traverse, A](f: F[A]) = (shape(f), contents(f))

      val tree = 'P'.node('O'.leaf, 'L'.leaf)
      val a = decompose(tree)

      val e = (shape(tree), contents(tree))
      a._2 should be (e._2)
      a._1.toString should be (e._1.toString)
    }

    "Traverse introduces a function called sequence. It works for all kinds of data structures." in {
      val a = List(1.some, 2.some).sequence
      val e = Some(List(1,2))
      a should be (e)

      val a2 = List(1.some, 2.some, none).sequence
      val e2 = none
      a2 should be (e2)

      val a3 = 1.success[String].node(2.success[String].leaf, 3.success[String].leaf)
      val a33 = a3.sequence[({type l[X] = Validation[String, X]})#l, Int]
      a33.isSuccess should be (true)

      val a4 = 1.success[String].node(2.success[String].leaf, "fred".failure[Int].leaf)
      val a44 = a4.sequence[({type l[X] = Validation[String, X]})#l, Int]
      a44.isSuccess should be (false)
    }

    "Its nice to have effectful traversals where the mapping part is independent of the" +
      "accummulation. This test mimics the use of a for loop with a mutable variable" +
      " outside of the loop, except here the effect is immutable like an immutable list that" +
      " returns a new list each time. " in {

      def collect[F[_]: Traverse, A, S, B](t: F[A])(f: A => B)(g: S => S) = t.traverseS[S, B] { a => State {(s: S) => (g(s), f(a))}}

      val loop = collect(List(1,2,3,4,5,6,7, 8)) {(_:Int) * 2} {(_:Int) + 1}
      val a = loop(1000)

      a should be ((1008, List(2,4,6,8,10,12,14,16)))
    }
  }
}
