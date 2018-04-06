package DFiant
import GlobalDesign._

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
  new a.matchdf {
    casedf(b) {

    }
    casedf_ {

    }
  }
}
