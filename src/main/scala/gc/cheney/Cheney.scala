package gc.cheney

/**
  * Created by wangzunhui on 2017/10/17.
  */
class obj(val p : Long){
  var ptr : Long = p
  var size : Long = 0L
  var forward : Long = 0L
  val children = List(3,4,5)
}

class Cheney {
  val N = 1024
  var tospace = N/2
  var fromspace = 0
  var allocptr = 0L
  var scanptr = 0L

  def allocate(n : Long) : Long = {
    if ((allocptr + n) > fromspace + N/2){
      collect()
    }

    if ((allocptr + n) > fromspace + N/2){
      println("out of memory")
      return 0L
    }

    val ptr = allocptr + n
    allocptr += n
    ptr
  }

  def swap() = {
    val t = fromspace
    fromspace = tospace
    tospace = t
  }

  def collect() = {
    //swap()
    allocptr = tospace
    scanptr = tospace

    val roots = List(1,3,5)
    roots.foreach(r => copy(r))

    while (scanptr < allocptr){
      val p = scanptr
      val o = new obj(p)

      o.children.foreach(r => copy(r))
      scanptr += o.size
    }
  }

  def copy(r:Long) : Long = {
    val o = new obj(r)
    if (o.forward == 0L){
      val oo = scanptr
      scanptr += o.size
      // copy the content of o to oo
      o.forward = oo
    }

    o.forward
  }
}

object Cheney {
  def main(args: Array[String]) : Unit = {

  }
}
