package nuctech.demo

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Created by wangzunhui on 2018/3/8.
  */
object MacroDemo {
  def hello(): Unit = macro helloImpl

  def helloImpl(c: blackbox.Context)(): c.Expr[Unit] = {
    import c.universe._
    reify { println("Hello World!") }
  }
}
