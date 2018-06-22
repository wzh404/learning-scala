package monads

/**
  * Created by wangzunhui on 2017/10/10.
  */
trait Functor[F[_]] {
  def map[A, B](fa:F[A])(f: A=>B) : F[B]

  def distribute[A, B](fab: F[(A, B)]) : (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e : Either[F[A], F[B]]) : F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] {
  def unit[A] (a: => A) : F[A]
  def flatMap[A, B](ma : F[A])(f: A => F[B]):F[B]

  def map[A, B](ma : F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma : F[A], mb : F[B])(f : (A, B) => C) : F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]) : F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_::_)) // A::List[A]

  def traverse[A, B](la : List[A])(f : A => F[B]) : F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_::_)) // B::List[B]

  def replicateM[A](n : Int, ma : F[A]) : F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_:: _)

  def compose[A, B, C](f : A => F[B], g: B=>F[C]) : A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def product[A, B](ma: F[A], mb: F[B]) : F[(A, B)] =
    map2(ma, mb)((_,_))

  def filterM[A](ms : List[A])(f: A => F[Boolean]) : F[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) =>
      compose(f, (b: Boolean) => if(b) map2(unit(x), y)(_::_) else y)(x))

  def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => ma, f)(())

  def __flatMap[A, B](ma : F[A])(f: A => F[B]) : F[B] =
    join(map(ma)(f))

  def _compose[A, B, C](f: A=>F[B], g : B=>F[C]) : A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma flatMap f
  }

  def main(args : Array[String]) : Unit = {

  }
}
