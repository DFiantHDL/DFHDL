package RISCV2

import DFiant._

class ScoreBoard(size : Int) extends DFDesign {
//  val maxScore : Int
//  private val scores = regsNum.map(ri => (ri, DFUInt.rangeUntil(maxScore).init(0).setName(s"x$ri")))
  def insertConn(regId : DFBits[5])(implicit ctx : DFDesign.Context) : Unit = {
//    scores.foreachdf(regId) {
//      case s => ifdf(s._2 != maxScore-1) {s._2 := s._2 + 1}
//    }
  }
  def searchConn(regId1 : DFBits[5], regId2 : DFBits[5])(implicit ctx : DFDesign.Context) : DFBool = ???
  def remove()(implicit ctx : DFDesign.Context) : Unit = ???
}
