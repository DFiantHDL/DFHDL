//package RingExample
//
//import DFiant._
//
//case class Ring (nodesNum : Int) {
//  private def nextNodeIdx(currentNodeIdx : Int): Int = {
//    (currentNodeIdx + 1) % nodesNum
//  }
//  private def prevNodeIdx(currentNodeIdx : Int): Int = {
//    (currentNodeIdx - 1) % nodesNum
//  }
//  private val ringNodes = {
//    Defs.nodesNum = nodesNum
//    val l2rFlitConn = Array.tabulate(nodesNum-1)(_ => DFFlit(nodesNum))
//    val r2lFlitConn = Array.tabulate(nodesNum-1)(_ => DFFlit(nodesNum))
//    val _ringNodes = Array.tabulate(nodesNum)(i => new RingNode(nodesNum, i) {
//        val leftInFlit    : DFFlit#IN  = l2rFlitConn(prevNodeIdx(i))
//        val leftOutFlit   : DFFlit#OUT = r2lFlitConn(prevNodeIdx(i))
//        val rightInFlit   : DFFlit#IN  = r2lFlitConn(nextNodeIdx(i))
//        val rightOutFlit  : DFFlit#OUT = l2rFlitConn(nextNodeIdx(i))
//      }
//    )
//    _ringNodes
//  }
//
//  def node(i : Int) = ringNodes(i)
//}
