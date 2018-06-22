package monads

/**
  * Created by wangzunhui on 2017/10/18.
  */
trait Applicative[F[_]] extends Functor[F]{
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def unit[A](a: => A):F[A]

  def map[A, B](fa: F[A])(f: A => B) : F[B] = {
    apply(unit(f))(fa)
  }
}

object Applicative {
  val listApplicative = new Applicative[List] {
    override def unit[Int](a: => Int): List[Int] = {
      List(a)
    }
  }

  def f(i : Int): Int = 2 * i

  def main(args: Array[String]):Unit = {
    println("---------------" + listApplicative.unit(f(2)))
  }
}
