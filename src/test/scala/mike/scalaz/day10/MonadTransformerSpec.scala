package mike.scalaz.day10

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class MonadTransformerSpec extends WordSpecLike with Matchers {

  trait TestContext {
    def myName(step: String): Reader[String, String] = Reader { step + ", I am " + _ }

    type ReaderTOption[A, B] = ReaderT[Option, A, B]
    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }

    def configure(key: String) = ReaderTOption[Map[String, String], String] { m => m.get(key) }

    def setupConnection = for {
      host <- configure("host")
      user <- configure("user")
      password <- configure("password")
    } yield (host, user, password)

    type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]

    object StateTReaderTOption extends StateTInstances with StateTFunctions {
      def apply[C, S, A](f: S => (S, A)) = new StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
        def apply(s: S) = f(s).point[({type l[X] = ReaderTOption[C, X]})#l]
      }

      def get[C, S]: StateTReaderTOption[C, S, S] = StateTReaderTOption { s => (s, s)}

      def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = StateTReaderTOption { _ => (s, ())}
    }
  }

  "ReaderMonad" must {
    "read from configuration once" in new TestContext {
      def localExample: Reader[String, (String, String, String)] = for {
        a <- myName("First")
        b <- myName("Second") >=> Reader { _ + "y" }
        c <- myName("Third")
      } yield (a, b, c)
      val a = localExample("Mike")
      val e = ("First, I am Mike", "Second, I am Mikey", "Third, I am Mike")
      a should be(e)
    }
  }

  "ReaderTransformer" must {
    "read from configuration once" in new TestContext {

      val goodConfig = Map(
        "host" -> "fred.com",
        "user" -> "mjlong",
        "password" -> "12234"
      )

      val a = setupConnection(goodConfig)
      val e = Some(("fred.com", "mjlong", "12234"))
      a should be(e)
    }
    "combine Reader's ability to read from configuration once with Option's ability to express failure" in new TestContext {

      val badConfig = Map(
        "host" -> "fred.com",
        "user" -> "mjlong"
      )
      val a = setupConnection(badConfig)
      a should be(empty)
    }
  }

  "StateTReaderOption" must {
    "stateT to represent state transfer on top of ReaderTOption" in new TestContext {

      type Stack = List[Int]

      type Config = Map[String, String]

      val pop = StateTReaderTOption[Config, Stack, Int] {
        case x :: xs => (xs, x )
      }


      def push(x: Int): StateTReaderTOption[Config, Stack, Unit]= {
        import StateTReaderTOption.get
        import StateTReaderTOption.put
        for {
          xs <- get[Config, Stack]
          r <- put(x :: xs)
        } yield r
      }

      def stackManip: StateTReaderTOption[Config, Stack, Int] =  for {
        _ <- push(3)
        a <- pop
        b <- pop
      } yield b

      val a = stackManip(List(5,8,2,1))(Map())
      val e = Some((List(8,2,1),5))
      a should be (e)
    }

  }

}
