//Parametric Polymorphism

def head[A](xs: List[A]): A = xs(0)

head(1 :: 2 :: 3 :: Nil)

//Subtype Polymorphism
trait Plus[A] {
  def plus(a2: A): A
}
def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)

//Now we can provide different definitions of plus for A. But its not
//flexible because trait Plus has to be mixed in at the time
//of defining the data type.  So it can't work for Int and String.


//Ad-hoc polymorphism -  You can provide an implicit conversion or
//implicit parameters for the

trait PlusAdHoc[A] {
  def plus(a1: A, a2: A) :A
}

def plusAH[A: PlusAdHoc](a1: A, a2: A): A = implicitly[PlusAdHoc[A]].plus(a1, a2)

//This is better because:
// 1) we can provide for different function defs for different types of A
// 2) we can provide function definitions to types(like String) without access
// to their source code.
// 3) the function definitions can be enabled or disabled in different scopes

import mike.Monoid._
sum(List("12","13","14","154111"))

