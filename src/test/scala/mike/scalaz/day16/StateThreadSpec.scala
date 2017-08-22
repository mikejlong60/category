package mike.scalaz.day16

import org.scalatest.{Matchers, WordSpecLike}

import scalaz._
import Scalaz._
import effect._
import ST._

class StateThreadSpec extends WordSpecLike with Matchers {

  trait TestContext {

    def e1[S] = for {
      x <- newVar[S](0)
      y <- x mod { _ + 1 }
    } yield x

    def e2[S]: ST[S, Int] = for {
      x <- e1[S]
      r <- x.read
    } yield r

    type ForallST[A] = Forall[({ type λ[S] = ST[S, A] })#λ]

  }

  "Understand how to use ScalaZ StateThread effect" must {
    "compose 2 effects" in new TestContext {
      val a = runST(new ForallST[Int] {
        def apply[S] = e2[S]
      })
      a should be(1)
    }

    "implement sieve of Erasthones using StateThread effect" in {
      def mapM[A, S, B](xs: List[A])(f: A => ST[S, B]): ST[S, List[B]] = Monad[({ type λ[α] = ST[S, α] })#λ].sequence(xs map f)

      def sieve[S](n: Int) = for {
        arr <- newArr[S, Boolean](n + 1, true)
        _ <- arr.write(0, false)
        _ <- arr.write(1, false)
        val nsq = (math.sqrt(n.toDouble).toInt + 1)
        _ <- mapM(1 |-> nsq) { i =>
          for {
            x <- arr.read(i)
            _ <- if (x) mapM(i * i |--> (i, n)) {
              j => arr.write(j, false)
            }
            else returnST[S, List[Boolean]] {
              Nil
            }
          } yield ()
        }
        r <- arr.freeze
      } yield r

      type ForallST[A] = Forall[({ type λ[S] = ST[S, A] })#λ]

      def prime(n: Int) =
        runST(new ForallST[ImmutableArray[Boolean]] {
          def apply[S] = sieve[S](n)
        }).toArray
          .zipWithIndex collect { case (true, x) => x }

      val a = prime(1000)
      a should be(Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89,
        97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197,
        199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
        331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449,
        457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593,
        599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727,
        733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
        877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997))
    }
  }
}