package typesample

/**
  * Created by wangzunhui on 2017/10/18.
  */
trait TypeSample {
  type not <: Boolean
  type equal[that <: TypeSample] <: Boolean

  type foo1[b <: TypeSample, c <: TypeSample] = b#equal[c]
  def foo[a <: TypeSample, b <: TypeSample](a : not, b : not) : equal[b]
}

//trait Boolean[Boolean];

object TypeSample {
  val t1 = new TypeSample{
    type not = Boolean
    type equal[TypeSample] = Boolean

    //def foo(a: Boolean, b:Boolean): Boolean = true
    override def foo[a <: TypeSample, b <: TypeSample](a: not, b: not): equal[b] = {
      val a : not = false
      val b : equal[TypeSample] = false

      b
    }
  }

  def main(args : Array[String]) : Unit = {

  }
}
