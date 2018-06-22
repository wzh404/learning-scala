package blockchain

import com.google.common.hash.Hasher
import com.google.common.hash.Hashing
/**
  * Created by wangzunhui on 2018/3/5.
  */
abstract class Node;
case class NonLeafNode(l:Node, r:Node) extends Node;
case class LeafNode(o:Object) extends Node;

object Merkle {
  var tree:Node = NonLeafNode(LeafNode("a"), LeafNode("b"));

  def hash(o:Object):Int = {
    o.hashCode()
  }

  def hash(node:Node) :Int = node match{
    case LeafNode(d) => hash(d)
    case NonLeafNode(l, r) => hash(l) + hash(r)
  }

  def main (args: Array[String] ): Unit = {
    println(hash(LeafNode("a")))
    println(hash(LeafNode("b")))
    println(hash(tree))
  }
}