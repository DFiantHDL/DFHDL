package DFiant.core

import DFiant._

object IfTest {

  import GlobalDesign._

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
