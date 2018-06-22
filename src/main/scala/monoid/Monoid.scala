package monoid

/**
  * Created by wangzunhui on 2017/9/29.
  */
trait Monoid[A] {
  // op(op(x,y), z) == op(x, op(y,z))
  def op(a1: A, a2:A): A

  // op(zero, x) == op(x, zero) == x
  def zero : A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero: String = ""
  }

  val intMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero = 1
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }

  val booleanMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero = true
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    def zero = Nil
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    def zero = (a: A) => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(f.curried)(z)
  }

  def foldMapV[A, B](is: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (is.length == 0)
      m.zero
    else if (is.length == 1)
      f(is(0))
    else {
      val (l, r) = is.splitAt(is.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (a1, a2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, y) => y
        }
      }

      def zero = None
    }

    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    def op(w1: WC, w2: WC): WC = {
      (w1, w2) match {
        case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
        case (Stub(x), Stub(y)) => Stub(x + y)
        case (Part(l, w, r), Stub(x)) => Part(l, w, r + x)
        case (Part(l, w, r), Part(rl, rw, rr)) =>
          Part(l, w + rw + (if ((r + rl).isEmpty()) 0 else 1), rr)
      }
    }

    def zero = Stub("")
  }

  def count(words: String): Int = {
    def wc(c: Char): WC = {
      if (c.isSpaceChar)
        Part("", 0, "")
      else
        Stub(c.toString)
    }

    // s is empty 0 else 1
    def unstub(s: String) = s.length min 1

    foldMapV(words.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) = {
        (a.op(x._1, y._1), b.op(x._2, y._2))
      }

      def zero = (a.zero, b.zero)
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B = {
        a => b.op(f(a), g(a))
      }

      def zero = a => b.zero
    }
  }

  def mapMergeMonoid[K,V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero){
          (acc, k) => acc.updated(k, v.op(a1.getOrElse(k, v.zero), a2.getOrElse(k, v.zero)))
        }

      override def zero: Map[K, V] = Map[K, V]()
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

  def main(args: Array[String]) = {
    val words = List("a", "b", "c", "d")
    println(words.foldLeft("")(stringMonoid.op))
    println(words.foldRight("")(stringMonoid.op))
    val f: (String => String) = (x => "4" )

    println(foldMap(words, stringMonoid)(f))
    println(ordered(IndexedSeq(1,2,3,4,5)))
    println(count("wangqi is a little boyd"))
    println(bag(Vector("a", "rose", "is", "a", "rose")))

    val m = productMonoid(intAddition, intAddition)
    val p = ListFoldable.foldMap(List(1,2,3,4))(a => (1, a))(m)
    println(p)
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as : F[A])(z: B)(f: (A, B) => B) : B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as : F[A])(z: B)(f: (B, A) => B):B =
    foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as : F[A])(f : (A => B))(mb:Monoid[B]) : B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}