package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._
protected[DFiant] sealed trait AllowUnchecked

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFEnum
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class EopEeqB[Kind <: DiSoOp.Kind, E <: Enum](e : E)(
    implicit ctx : DFComponent.Context[EopEeqB[Kind, E]]
  ) extends DFComponent[EopEeqB[Kind, E]] {
    final lazy val inLeft = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val inRight = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val outResult = ??? //DFBool() <> OUT
  }
  protected[DFiant] type `E==E`[E <: Enum] = EopEeqB[DiSoOp.Kind.==, E]
  implicit def `evE==E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E==E`[E]]
  protected[DFiant] type `E!=E`[E <: Enum] = EopEeqB[DiSoOp.Kind.!=, E]
  implicit def `evE!=E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E!=E`[E]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


object DFBasicLib {
  protected implicit object AllowUnchecked extends AllowUnchecked

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    class UopUeqU[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
      implicit ctx : DFComponent.Context[UopUeqU[Kind]]
    ) extends DFComponent[UopUeqU[Kind]] {
      final val inLeft = DFUInt.unchecked(leftWidth) <> IN
      final val inRight = DFUInt.unchecked(rightWidth) <> IN
      final val outResult = DFUInt.unchecked(resultWidth) <> OUT
    }

    protected[DFiant] type `Comp+` = UopUeqU[DiSoOp.Kind.+]
    implicit def `ev+`(implicit ctx : Implementation.Context) : Implementation[`Comp+`]
    protected[DFiant] type `Comp-` = UopUeqU[DiSoOp.Kind.-]
    implicit def `ev-`(implicit ctx : Implementation.Context) : Implementation[`Comp-`]
    protected[DFiant] type `Comp*` = UopUeqU[DiSoOp.Kind.*]
    implicit def `ev*`(implicit ctx : Implementation.Context) : Implementation[`Comp*`]

    class UopUeqB[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int)(
      implicit ctx : DFComponent.Context[UopUeqB[Kind]]
    ) extends DFComponent[UopUeqB[Kind]] {
      final val inLeft = DFUInt.unchecked(leftWidth) <> IN
      final val inRight = DFUInt.unchecked(rightWidth) <> IN
      final val outResult = DFBool() <> OUT
    }

    protected[DFiant] type `Comp==` = UopUeqB[DiSoOp.Kind.==]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    protected[DFiant] type `Comp!=` = UopUeqB[DiSoOp.Kind.!=]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
    protected[DFiant] type `Comp<` = UopUeqB[DiSoOp.Kind.<]
    implicit def `ev<`(implicit ctx : Implementation.Context) : Implementation[`Comp<`]
    protected[DFiant] type `Comp>` = UopUeqB[DiSoOp.Kind.>]
    implicit def `ev>`(implicit ctx : Implementation.Context) : Implementation[`Comp>`]
    protected[DFiant] type `Comp<=` = UopUeqB[DiSoOp.Kind.<=]
    implicit def `ev<=`(implicit ctx : Implementation.Context) : Implementation[`Comp<=`]
    protected[DFiant] type `Comp>=` = UopUeqB[DiSoOp.Kind.>=]
    implicit def `ev>=`(implicit ctx : Implementation.Context) : Implementation[`Comp>=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}