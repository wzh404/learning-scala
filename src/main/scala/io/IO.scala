package io

import scala.io.StdIn

/**
  * Created by wangzunhui on 2018/2/22.
  */
sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = f(self.run).run
    }

  def map2[B, C](iob:IO[B])(f : (A, B) => C) : IO[C] ={
    new IO[C] {
      def run = f(self.run, iob.run)
    }
  }

}

object IO {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def rd: IO[String] = IO {
    StdIn.readLine()
  }

  def pr(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def converter: IO[Unit] = for {
    _ <- pr("enter a temperature")
    d <- rd.map(_.toDouble)
    _ <- pr(d.toString)
  } yield ()

  val echo = rd.flatMap(pr)
  val readInt = rd.map(_.toInt)
  val readInts = readInt.map2(readInt)((_,_))
}