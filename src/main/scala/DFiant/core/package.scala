package DFiant

//import singleton.ops._
//import singleton.twoface._

package object core {
  //////////////////////////////////////////////////////////////////////////////////////////
  // Ports
  //////////////////////////////////////////////////////////////////////////////////////////
  type OUT = DFDir.OUT.type
  type IN = DFDir.IN.type

  type <>[DF <: DFAny, DIR <: DFDir] = DFPort[DF, DIR]
  //////////////////////////////////////////////////////////////////////////////////////////

  type BitsRange = internals.BitsRange
  val BitsRange = internals.BitsRange

  implicit class ElseIfClauseBuilder(cond : DFBool){
    def apply(block : => Unit) : ElseIfClause = new ElseIfClause(cond, block)
  }

}
