package mike.scalaz.day8.mymonad

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz.Equal
import scalaz.Scalaz._
import scalaz._, Scalaz._, scala.language.higherKinds

class MyMonadSpec extends WordSpecLike with Matchers {


  "MyMonad" must {
    "flipThree" in  {
      import mike.scalaz.day8.mymonad.fred._
      val a = flipThree

      val e = Prob(List((true,0.025), (false,0.225), (false,0.025), (false,0.225), (false,0.025), (false,0.225), (false,0.025), (false,0.225)))
      a should be (e)
    }

    "map over a list" in {
      val a = Prob(List((12, 0.5), (14, 0.25), (16, 0.25))) map (x => x + 12)

      val e = Prob(List((12 + 12, 0.5), (14 + 12, 0.25), (16 + 12, 0.25)))
      a should be (e)
    }
  }
}
