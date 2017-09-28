package traits

/**
  * Created by wangzunhui on 2017/9/22.
  */
trait Ord {
  def < (that:Any) : Boolean
  def <= (that:Any): Boolean = (this < that) || (this == that)
  def > (that: Any): Boolean = !(this <= that)
  def >= (that : Any): Boolean = !(this < that)
}

class Date(y : Int, m:Int, d: Int) extends Ord{
  def year = y
  def month = m
  def day = d

  override def <(that: Any): Boolean = {
    (that.isInstanceOf[Date]) && {
      val o = that.asInstanceOf[Date]
      o.day == day && o.month == month && o.year == year
    }
  }
}