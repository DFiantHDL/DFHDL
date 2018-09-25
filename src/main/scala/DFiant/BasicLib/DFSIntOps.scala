package DFiant.BasicLib

import DFiant._
import internals._

object DFSIntOps {
  class Arithmetic[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[Arithmetic[Kind]], kind : Kind
  ) extends DFComponent[Arithmetic[Kind]] {
    final val inLeft = DFSInt(leftWidth) <> IN
    final val inRight = DFSInt(rightWidth) <> IN
    final val outResult = DFSInt(resultWidth) <> OUT
    override protected def foldedRun: Unit = {
      val func = kind match {
        case _: DiSoOp.Kind.+ => DFSInt.Token.+
        case _: DiSoOp.Kind.- => DFSInt.Token.-
        case _: DiSoOp.Kind.* => DFSInt.Token.*
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
      setInitFunc(outResult)(LazyBox.Args2(fullName)(func, getInit(inLeft), getInit(inRight)))
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
    final val inLeft = DFSInt(leftWidth) <> IN
    final val inRight = DFSInt(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp==` = Relational[DiSoOp.Kind.==]
  type `Comp!=` = Relational[DiSoOp.Kind.!=]
  type `Comp<`  = Relational[DiSoOp.Kind.<]
  type `Comp>`  = Relational[DiSoOp.Kind.>]
  type `Comp<=` = Relational[DiSoOp.Kind.<=]
  type `Comp>=` = Relational[DiSoOp.Kind.>=]

  class BitShift[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[BitShift[Kind]], kind : Kind
  ) extends DFComponent[BitShift[Kind]] {
    final val inLeft = DFSInt(leftWidth) <> IN
    final val inRight = DFUInt(rightWidth) <> IN
    final val outResult = DFSInt(leftWidth) <> OUT
    override protected def foldedRun: Unit = {
      val func = kind match {
        case _: DiSoOp.Kind.<< => DFSInt.Token.<<
        case _: DiSoOp.Kind.>> => DFSInt.Token.>>
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
      setInitFunc(outResult)(LazyBox.Args2(fullName)(func, getInit(inLeft), getInit(inRight)))
    }
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp<<` = BitShift[DiSoOp.Kind.<<]
  type `Comp>>` = BitShift[DiSoOp.Kind.>>]

}
