package mike

trait GenericCategory[->>[_, _]] {
  def id[A]: A ->> A
  def compose[A, B, C](g: B ->> C, f: A ->> B): A ->> C
}

trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def fmap[A, B](f: A ->> B): F[A] ->>> F[B]
}

trait Functor[F[_]] extends GenericFunctor[Function, Function, F] {
  final def fmap[A, B](as: F[A])(f: A => B): F[B] =
    fmap(f)(as)
}

object Functor {

  def fmap[A, B, F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.fmap(as)(f)

  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] =
      as => as map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] =
      o => o map f
  }

  implicit object Function0Functor extends Functor[Function0] {
    def fmap[A, B](f: A => B): Function0[A] => Function0[B] = a => () => f(a())
  }

}

