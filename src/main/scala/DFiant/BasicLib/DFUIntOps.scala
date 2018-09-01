package DFiant.BasicLib
import DFiant._

object DFUIntOps {
  class Arithmetic[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[Arithmetic[Kind]], kind : Kind
  ) extends DFComponent[Arithmetic[Kind]] {
    final val inLeft = DFUInt(leftWidth) <> IN
    final val inRight = DFUInt(rightWidth) <> IN
    final val outResult = DFUInt(resultWidth) <> OUT
    override protected def foldedRun: Unit = {
      kind match {
        case _: DiSoOp.Kind.+ => setInitFunc(outResult)(DFUInt.Token.+(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.- => setInitFunc(outResult)(DFUInt.Token.-(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.* => setInitFunc(outResult)(DFUInt.Token.*(getInit(inLeft), getInit(inRight)))
        case _ =>
      }
    }
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp+` = Arithmetic[DiSoOp.Kind.+]
  type `Comp-` = Arithmetic[DiSoOp.Kind.-]
  type `Comp*` = Arithmetic[DiSoOp.Kind.*]

  class Relational[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
  ) extends DFComponent[Relational[Kind]] {
    final val inLeft = DFUInt(leftWidth) <> IN
    final val inRight = DFUInt(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
    override protected def foldedRun: Unit = {
      kind match {
        case _: DiSoOp.Kind.== => setInitFunc(outResult)(DFUInt.Token.==(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.!= => setInitFunc(outResult)(DFUInt.Token.!=(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.< => setInitFunc(outResult)(DFUInt.Token.<(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.> => setInitFunc(outResult)(DFUInt.Token.>(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.<= => setInitFunc(outResult)(DFUInt.Token.<=(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.>= => setInitFunc(outResult)(DFUInt.Token.>=(getInit(inLeft), getInit(inRight)))
        case _ =>
      }
    }
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp==` = Relational[DiSoOp.Kind.==]
  type `Comp!=` = Relational[DiSoOp.Kind.!=]
  type `Comp<`  = Relational[DiSoOp.Kind.<]
  type `Comp>`  = Relational[DiSoOp.Kind.>]
  type `Comp<=` = Relational[DiSoOp.Kind.<=]
  type `Comp>=` = Relational[DiSoOp.Kind.>=]

}
