//Parametric Polymorphism

def head[A](xs: List[A]): A = xs(0)

head(1 :: 2 :: 3 :: Nil)

//Subtype Polymorphism
trait Plus[A] {
  def plus(a2: A): A
}
def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)

//Now we can provide different definitions of plus for A. But its not
//flexible because you since trait Plus has to be mixed in at the time
//of defining the data type.  So it can't work for Int and String.


//Ad-hoc polymorphism -  You can provide an implicit conversion or
//implicit parameters for the

trait PlusAdHoc[A] {
  def plus(a1: A, a2: A) :A
}

def plusAH[A: PlusAdHoc](a1: A, a2: A): A = implicitly[PlusAdHoc[A]].plus(a1, a2)

//This is better because:
// 1) we can provide for different function defs for different types of A
// 2) we cxan peovide function definitions to types(like String) without access
// to their source code.
// 3) the function definitions can be enabled ot disabled in different scopes

def sum(xs: List[Int]): Int = xs.foldLeft(0) {_ + _}
sum(List(12,13,14,154))

//Here is a Monoid. Its a type for which there exists a function mappend that
//produces another type of the same set and also has a function which produces
//a zero.

object IntMonoid {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

//Then we canz pull it in and generalize the sum function above.
def sumG(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)
sumG(List(12,13,14,154))

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object IntMonoid2 extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

object StringMonoid2 extends Monoid[String] {
  def mappend(a: String, b: String): String = s"$a$b"
  def mzero: String = ""
}

def sumG2[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)
sumG2(List(12,13,14,154), IntMonoid2)


sumG2(List("12","13","14","154"), StringMonoid2)


