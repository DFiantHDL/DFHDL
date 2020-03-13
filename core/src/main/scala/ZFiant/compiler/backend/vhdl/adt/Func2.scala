package ZFiant.compiler.backend.vhdl.adt

import DFiant.internals.StringExtras

////////////////////////////////////////////////////////////////////////////////////////////////////////
// Infix function between two arguments
////////////////////////////////////////////////////////////////////////////////////////////////////////
final case class Func2(leftArg : String, opStr : String, rightArg : String) extends Statement {
  override def toString: String = s"${leftArg.applyBrackets()} $opStr ${rightArg.applyBrackets()}"
}
