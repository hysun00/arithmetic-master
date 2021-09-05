package addition.prefixadder.graph

import scala.math.Ordered.orderingToOrdered

/**
  * The nodes of a prefix graph is known as PrefixNode.
  * There are several nodes in the same logic level of a prefix graph.
  * A node has several fan-ins and fan-outs.
  * Some nodes in the implement certain logic operations, mapping inputs from fan-ins into some different outputs,
    while all the others act as a buffer.
  * The logic depth of a node is represented by level.
  * Fan-ins, also the father nodes of a prefix node, are called fathers.
  * Output of the node is called prefixData.
  * The nodes of the same level are sortable with the help of index
  * */

case class PrefixNode(level: Int, fathers: Set[PrefixNode], prefixData: Set[Int], index: Int)
     extends Ordered[PrefixNode] {
  val bit:Int = prefixData.max
  override def toString: String = s"Node$level-$bit-$index"
  override def compare(that: PrefixNode): Int = (this.bit, this.level).compare(that.bit, that.level)
}


/**
  * Major construction method of PrefixNode is defined in this companion object.
  * */
object PrefixNode {
  var index = 0

  /**
    * Receives a variable numbers of PrefixNodes, and gernerate a child PrefixNode
    *
    * @param father a variable number of PrefixNodes
    *
    * @return a PrefixNode
    **/
  def apply(father: PrefixNode*): PrefixNode = {
    require(father.map(_.prefixData).reduce(_ intersect _).isEmpty, "father's prefixData has same prefixes")
    require(
      father.flatMap(_.prefixData).toList.sorted.zipWithIndex.map { case (idx, ele) => ele - idx }.toSet.size == 1,
      "prefixData is not continues"
    )
    new PrefixNode(father.map(_.level).max + 1, father.sorted.toSet, father.flatMap(_.prefixData).toSet, indexInc)
  }

  def indexInc = {
    index = index + 1
    index - 1
  }

  def apply(bit: Int): PrefixNode = {
    new PrefixNode(0, Set(), Set(bit), indexInc)
  }
}
