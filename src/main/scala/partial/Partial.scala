package partial

/**
  * Created by wangzunhui on 2017/9/29.
  */
object Partial {
  val p : PartialFunction[Int, String] = {case 1 => "one"}
  val isOdd: PartialFunction[Int, String] = {
    case x if x % 2 == 1 => x + " is odd"
  }

  def main (args: Array[String] ): Unit = {
    p.andThen(println)(1)
    println(p.applyOrElse(1, isOdd))
    p.orElse(isOdd)(3)
  }
}
