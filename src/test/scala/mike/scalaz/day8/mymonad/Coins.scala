package mike.scalaz.day8.mymonad

import scalaz._, Scalaz._, scala.language.higherKinds

sealed trait Coin

case object Heads extends Coin
case object Tails extends Coin

object fred {
  implicit val coinEqual: Equal[Coin] = Equal.equalA
  def coin: Prob[Coin] = Prob(Heads -> 0.5 :: Tails -> 0.5 :: Nil)

  def loadedCoin: Prob[Coin] = Prob(Heads -> 0.1 :: Tails -> 0.9 :: Nil)

  def flipThree: Prob[Boolean] = coin.flatMap(a => coin.flatMap(b => loadedCoin.map(c => List(a, b, c) all (_ === Heads))))
}