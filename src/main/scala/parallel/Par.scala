package parallel

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * Created by wangzunhui on 2017/11/22.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]) : Future[A] = a(es)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => {
      es.submit(new Callable[A] {
        override def call(): A = a(es).get()
      })
    }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es : ExecutorService) => {
      val af = a(es).get
      val bf = b(es).get
      UnitFuture(f(af, bf))
    }

  def sum0(ints : IndexedSeq[Int]) : Int = {
    ints.foldLeft(0)((a, b) => a + b)
  }

  def sum1(ints : IndexedSeq[Int]) : Int = {
    if (ints.size <= 1){
      ints.headOption.getOrElse(0)
    }
    else {
      val (l, r) = ints.splitAt(ints.length / 2);
      sum1(l) + sum1(r)
    }
  }

  def main(args : Array[String]) : Unit = {
    val a = sum1(IndexedSeq[Int](1,2,3,4,5))
    println(a)
  }
}
