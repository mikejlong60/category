package mike.scalaz.day9

import org.scalatest.{Matchers, WordSpecLike}
import scala.language.higherKinds
import scalaz._, Scalaz._, scala.language.higherKinds
import scalaz.Tree

class TreeSpec extends WordSpecLike with Matchers {

  trait TestContext {

    def freeTree: Tree[Char] =
      'P'.node(
        'O'.node(
          'L'.node('N'.leaf, 'T'.leaf),
          'Y'.node('S'.leaf, 'A'.leaf)
        ),
        'L'.node(
          'W'.node('C'.leaf, 'R'.leaf),
          'A'.node('A'.leaf, 'C'.leaf)
        )
      )

    def changeToP(tree: Tree[Char]): Tree[Char] = tree match {
      case Tree.Node(x, Stream(
        l, Tree.Node(y, Stream(
          Tree.Node(_, Stream(m, n)), r)))) =>
        x.node(l, y.node('P'.node(m, n), r))
    }

  }

  "Tree" must {
    "allow you to move around using flatMap(>>=)" in new TestContext {
      val a = freeTree.loc.getChild(2) >>= { _.getChild(1) } >>= { _.getLabel.some }
      val e = Some('W')
      a should be(e)
    }

    "allow you to modify a label as you move around using flatMap(>>=)" in new TestContext {
      val newFocus = freeTree.loc.getChild(2) >>= { _.getChild(1) } >>= { _.modifyLabel({ _ => 'P' }).some }
      val a = newFocus.get.tree.loc.getLabel.some
      val e = Some('P')
      a should be(e)
    }
  }
}
