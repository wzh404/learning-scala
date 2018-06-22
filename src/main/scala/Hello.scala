import scala.annotation.tailrec

/**
  * Created by wangzunhui on 2017/9/22.
  */
object Hello {
  def mul3(x:Int, y:Int, z:Int) : Int = x * y * z

  @tailrec def acc(r: Int, index: Int): Int = {
    if (index == 0)
      r
    else
      acc(r+index, index - 1)
  }

  var flag: Boolean = true
  def useOrNotUse(x: Int, y: => Int) = {
    flag match{
      case true => x
      case false => x + y
    }
  }

  def throwableLeft[T](block: => T) : Either[Throwable, T] = {
    try{
      Right(block)
    } catch {
      case ex => Left(ex)
    }

  }

  def lift[A, B](f : A => B) : Option[A] => Option[B] =
    _ map(f)

  def main(args : Array[String]) : Unit = {
    println("hello world")

    val m = Map(1 -> "one")
    println(m(1))
    val m1 = m.updated(1, "One")
    println(m(1))
    println(m ++ m1)
    println(m zip m1)

    val f1 = (mul3 _).curried
    println(f1)
    println(f1(2))
    println(f1(2)(3)(4))

    implicit def fun(x: String) = x.toInt
    println(math.max(4, "20"))

    class Container[A <% Int] {
      def addIt(x: A) = 123 + x
    }
    println((new Container[String]).addIt("124"))

    println(acc(0, 100))
    println(Arr(1))
    println(Fun(4))

    def f(x:String) : String = "f(" + x + ")"
    def g(x:String) : String = "g(" + x + ")"

    def fg = (f _) compose (g _)
    def gf = (f _) andThen (g _)
    println(fg("a"))
    println(gf("a"))

    def implicitly[T](implicit e: T): T = e
    implicitly[Int =:= Int]
    implicitly[Int <:< AnyVal]

    println(useOrNotUse(1, 2))
    flag = false
    println(useOrNotUse(1, 2))

    val s:String = "hello world"
    throwableLeft{s.toUpperCase} match {
      case Left(e) => e.printStackTrace
      case Right(a) => println(a)
    }
  }
}

object Arr {
  val arr = Array(1,2,3,4,5)
  def apply(i : Int) = arr(i)
  def update(i: Int, v : Int) = {arr(i) = v}
}

object Fun extends Function1[Int, Int] {
  override def apply(v1: Int): Int = v1 * v1
}
