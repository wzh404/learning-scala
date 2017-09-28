package caseclass

/**
  * Created by wangzunhui on 2017/9/22.
  */
abstract class Tree
case class Sum(l : Tree, r : Tree) extends Tree
case class Var(n: String) extends Tree
case class Const(v : Int) extends Tree


object c {
    type Evnironment = String => Int
    val env: Evnironment = {case "x" => 5 case "y" => 7}
    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Var("y"), Const(7)))

    def eval(t : Tree, env: Evnironment) : Int = t match {
      case Sum(l, r) => eval(l, env) + eval(r, env)
      case Var(n) => env(n)
      case Const(v) => v
    }

    def main (args: Array[String] ): Unit = {
      println("expression: " + exp)
      println("eval: " + eval(exp, env))
    }
  }

