package mike.scalaz.day7

import org.scalatest.{Matchers, WordSpecLike}
import scalaz.Scalaz._

/**
 * Created by mjlong on 4/5/17.
 */
class ApplicativeBuilderSpec extends WordSpecLike with Matchers {

  "Applicative Builder must" must {
    "allow you to compose a whole list of functors." in {
      val a = ((1 + 12).some |@| (2 + 13).some) { _ + _ }
      val e = Some(28)
      a should be(e)
    }

    "allow you to compose List as a functor.  This is just like a for comprehension" in {
      val a = (List(1,2) |@| List(4,5) |@| List(7,8)) { (x, y, z) => {
        println(s"$x, $y, $z")
        x + y + z
      }}
      val e = List(12,13,13,14,13,14,14,15)
      a should be(e)
    }

    "another example of composing lots of functors" in {
      val a = (3.some |@| 4.some |@| 5.some |@| 6.some)((v1, v2, v3, v4) => v1 + v2 + v3 + v4)
      val e = Some(18)
      a should be(e)
    }

    "allow you to define a list of Applicatives but not apply them until later" in {
      val f = ({(_: Int) * 2} |@| {(_: Int) + 10} |@| {(_:Int) + 20}) {_ + _ - _}
      val a = f(12)
      val e = 14
      a should be(e)
    }
  }
}
