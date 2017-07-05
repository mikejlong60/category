package mike.scalaz.day10

import org.scalatest.{Matchers, WordSpecLike}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

class MonadTransformerSpec extends WordSpecLike with Matchers {

  trait TestContext {
    def myName(step: String): Reader[String, String] = Reader {step + ", I am " + _}

    type ReaderTOption[A, B] = ReaderT[Option, A, B]
    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B] (f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }

    def configure(key: String) = ReaderTOption[Map[String, String], String] {m => m.get(key)}

    def setupConnection = for {
      host <- configure("host")
      user <- configure("user")
      password <- configure("password")
    } yield (host, user, password)

  }

  "ReaderMonad" must {
    "read from configuration once" in new TestContext {
      def localExample: Reader[ String, (String, String, String)] = for {
        a <- myName("First")
        b <- myName("Second") >=> Reader {_ + "y"}
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

      val badConfig = Map(
        "host" -> "fred.com",
        "user" -> "mjlong"
      )

      val a = setupConnection(goodConfig)
      val e = Some(("fred.com","mjlong","12234"))
      a should be (e)
    }
    "combine Reader's ability to read from configuration once with Option's ability to express failure" in new TestContext {

      val badConfig = Map(
        "host" -> "fred.com",
        "user" -> "mjlong"
      )
      val a = setupConnection(badConfig)
      a should be (empty)
    }
  }
}
