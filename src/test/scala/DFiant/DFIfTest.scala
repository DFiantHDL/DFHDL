package DFiant
import GlobalDesign._
import singleton.ops.GetArg0

object IfTest {
  val a = DFBool()
  val b = DFUInt(8)
  val c = a && a
  val d : DFBool = b >= 1 && b < 2
  b := b

//  implicit class ElseIfClauseBuilder(cond : DFBool){
//    def apply(block : => Unit): ElseIfClause = ??? // new ElseIfClause(cond, block)
//    def && (that : DFBool) : ElseIfClauseBuilder = ???
//  }

  ifdf (a) {

  }.elseifdf (b != b) {

  }.elsedf {

  }
}

object MatchTest {
  val a = DFUInt[8]
  val b = DFUInt[8]
  val aa = 0 to 5

  a.casedf(a) {

//    casedf(b) {
//
//    }
//    casedf_ {
//
//    }
  }.casedf(b, b, b, b, b) {

  }.casedf(a) {

  }.case_ {

  }
}

import DFiant.internals._
object vb {
  val a = 1 To 8
  for (i <- a) {

  }

}