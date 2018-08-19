package DFiant.basiclib

import DFiant._

object DFSIntOps {
  class Arithmetic[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[Arithmetic[Kind]], kind : Kind
  ) extends DFComponent[Arithmetic[Kind]] {
    final val inLeft = DFSInt(leftWidth) <> IN
    final val inRight = DFSInt(rightWidth) <> IN
    final val outResult = DFSInt(resultWidth) <> OUT
    override protected def foldedRun: Unit = {
      kind match {
        case _: DiSoOp.Kind.+ => setInitFunc(outResult)(DFSInt.Token.+(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.- => setInitFunc(outResult)(DFSInt.Token.-(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.* => setInitFunc(outResult)(DFSInt.Token.*(getInit(inLeft), getInit(inRight)))
        case _ =>
      }
    }
  }

  type `Comp+` = Arithmetic[DiSoOp.Kind.+]
  type `Comp-` = Arithmetic[DiSoOp.Kind.-]
  type `Comp*` = Arithmetic[DiSoOp.Kind.*]

  class Relational[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
  ) extends DFComponent[Relational[Kind]] {
    final val inLeft = DFSInt(leftWidth) <> IN
    final val inRight = DFSInt(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
  }

  type `Comp==` = Relational[DiSoOp.Kind.==]
  type `Comp!=` = Relational[DiSoOp.Kind.!=]
  type `Comp<`  = Relational[DiSoOp.Kind.<]
  type `Comp>`  = Relational[DiSoOp.Kind.>]
  type `Comp<=` = Relational[DiSoOp.Kind.<=]
  type `Comp>=` = Relational[DiSoOp.Kind.>=]

}
