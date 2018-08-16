package DFiant.basiclib
import DFiant._

object DFBitsOps {
  class Bitwise[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[Bitwise[Kind]], kind : Kind
  ) extends DFComponent[Bitwise[Kind]] {
    final val inLeft = DFBits(leftWidth) <> IN
    final val inRight = DFBits(rightWidth) <> IN
    final val outResult = DFBits(resultWidth) <> OUT
  }

  type `Comp|` = Bitwise[DiSoOp.Kind.|]
  type `Comp&` = Bitwise[DiSoOp.Kind.&]
  type `Comp^` = Bitwise[DiSoOp.Kind.^]

  class Relational[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
  ) extends DFComponent[Relational[Kind]] {
    final val inLeft = DFBits(leftWidth) <> IN
    final val inRight = DFBits(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
  }

  type `Comp==` = Relational[DiSoOp.Kind.==]
  type `Comp!=` = Relational[DiSoOp.Kind.!=]

}
