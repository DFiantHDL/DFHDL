package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._
protected[DFiant] sealed trait AllowUnchecked

trait DFBasicLib {
  protected implicit object AllowUnchecked extends AllowUnchecked
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class UopUeqU[Kind <: DiSoOp.Kind, LW, RW, OW](
    val leftWidth : TwoFace.Int[LW], val rightWidth : TwoFace.Int[RW], val resultWidth : TwoFace.Int[OW])(
    implicit ctx : DFComponent.Context[UopUeqU[Kind, LW, RW, OW]]
  ) extends DFComponent[UopUeqU[Kind, LW, RW, OW]] {
    final val inLeft = DFUInt.unchecked(leftWidth) <> IN
    final val inRight = DFUInt.unchecked(rightWidth) <> IN
    final val outResult = DFUInt.unchecked(resultWidth) <> OUT
  }

  protected[DFiant] type `U+U`[LW, RW, OW] = UopUeqU[DiSoOp.Kind.+, LW, RW, OW]
  implicit def `evU+U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U+U`[LW, RW, OW]]
  protected[DFiant] type `U-U`[LW, RW, OW] = UopUeqU[DiSoOp.Kind.-, LW, RW, OW]
  implicit def `evU-U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U-U`[LW, RW, OW]]
  protected[DFiant] type `U*U`[LW, RW, OW] = UopUeqU[DiSoOp.Kind.*, LW, RW, OW]
  implicit def `evU*U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U*U`[LW, RW, OW]]

  class UopUeqB[Kind <: DiSoOp.Kind, LW, RW](
    val leftWidth : TwoFace.Int[LW], val rightWidth : TwoFace.Int[RW])(
    implicit ctx : DFComponent.Context[UopUeqB[Kind, LW, RW]]
  ) extends DFComponent[UopUeqB[Kind, LW, RW]] {
    final val inLeft = DFUInt.unchecked(leftWidth) <> IN
    final val inRight = DFUInt.unchecked(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
  }

  protected[DFiant] type `U==U`[LW, RW] = UopUeqB[DiSoOp.Kind.==, LW, RW]
  implicit def `evU==U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U==U`[LW, RW]]
  protected[DFiant] type `U!=U`[LW, RW] = UopUeqB[DiSoOp.Kind.!=, LW, RW]
  implicit def `evU!=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U!=U`[LW, RW]]
  protected[DFiant] type `U<U`[LW, RW] = UopUeqB[DiSoOp.Kind.<, LW, RW]
  implicit def `evU<U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U<U`[LW, RW]]
  protected[DFiant] type `U>U`[LW, RW] = UopUeqB[DiSoOp.Kind.>, LW, RW]
  implicit def `evU>U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U>U`[LW, RW]]
  protected[DFiant] type `U<=U`[LW, RW] = UopUeqB[DiSoOp.Kind.<=, LW, RW]
  implicit def `evU<=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U<=U`[LW, RW]]
  protected[DFiant] type `U>=U`[LW, RW] = UopUeqB[DiSoOp.Kind.>=, LW, RW]
  implicit def `evU>=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U>=U`[LW, RW]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////



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


