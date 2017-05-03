package mike.scalaz.day3

import java.util.{Calendar, Date}

import org.scalatest.{Matchers, WordSpecLike}

import scalaz.Tag

//import scala.language.higherKinds
//import scalaz.Scalaz._
//import scalaz._
//import scalaz.syntax.Ops

class TaggedTypesSpec extends WordSpecLike with Matchers {

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]

  trait Day

  trait Epoch

  type Epochtime = Long @@ Epoch
  type Daytime = Long @@ Day

  def daytime(i: java.lang.Long): Daytime = i.asInstanceOf[Daytime]

  def epochtime(i: java.lang.Long): Epochtime = i.asInstanceOf[Epochtime]

  def epochtimeToDaytime(time: Long): Daytime = {
    val calendar = Calendar.getInstance
    calendar.setTime(new Date(time))
    daytime(((calendar.get(Calendar.HOUR_OF_DAY) * 60L +
      calendar.get(Calendar.MINUTE)) * 60 +
      calendar.get(Calendar.SECOND)) * 1000 +
      calendar.get(Calendar.MILLISECOND))
  }

//  override implicit def toEpochtimeDisplay(t: Epochtime) = new EpochtimeDisplay(t)
//
//  case class EpochtimeDisplay(time: Epochtime) {
//    def hhmm = hhmmFormat.format(new Date(time))
//  }

  case class DaytimeRange(start: Daytime, end: Daytime)

  "Tagged Types" must {
    "update only the last element for tuples" in {
      val now = System.currentTimeMillis()
      val twelveHours = now + (1000 * 60 * 60 * 12)
      val r = DaytimeRange(epochtimeToDaytime(now), epochtimeToDaytime(twelveHours))
      true should be(true)
    }
  }
}