package alg

/**
  * Created by wangzunhui on 2018/4/4.
  */
class Node(val x : Int, val y: Int, var p: Node) {
  var f: Int = 0
  var g: Int = 0
  var h: Int = 0
}

object AStar {
  var open : Set[Node] = Set()
  var close : Set[Node]= Set()
  val a = Array(
    Array(1,2,4),
    Array(2,3,5))

  def main(args: Array[String]): Unit = {

    val start = new Node(1,1, null);
    open += start
    println(start.f)
    println(a(0)(2))
  }
}
