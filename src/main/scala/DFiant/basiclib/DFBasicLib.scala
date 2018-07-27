package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {
  implicit val basicLib : DFBasicLib = this

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class UopUeqU[Kind <: DiSoOp.Kind](leftWidth : Int, rightWidth : Int, resultWidth : Int)(
    implicit ctx : DFComponent.Context[UopUeqU[Kind]]
  ) extends DFComponent[UopUeqU[Kind]] {
    final lazy val inLeft = DFUInt(leftWidth) <> IN
    final lazy val inRight = DFUInt(rightWidth) <> IN
    final lazy val outResult = DFUInt(resultWidth) <> OUT
  }

  protected type UopUeqB[Kind <: DiSoOp.Kind, LW, RW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFBool]

  protected[DFiant] type `U+U` = UopUeqU[DiSoOp.Kind.+]
  implicit def `evU+U`(implicit blk : DFBlock) : Implementation[`U+U`]

  protected[DFiant] type `U-U` = UopUeqU[DiSoOp.Kind.-]
  implicit def `evU-U`(implicit blk : DFBlock) : Implementation[`U-U`]

  protected[DFiant] type `U*U` = UopUeqU[DiSoOp.Kind.*]
  implicit def `evU*U`(implicit blk : DFBlock) : Implementation[`U*U`]

  protected[DFiant] type `U==U`[LW, RW] = UopUeqB[DiSoOp.Kind.==, LW, RW]
  implicit def `evU==U`[LW, RW](implicit blk : DFBlock) : Implementation[`U==U`[LW, RW]]
  protected[DFiant] type `U!=U`[LW, RW] = UopUeqB[DiSoOp.Kind.!=, LW, RW]
  implicit def `evU!=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U!=U`[LW, RW]]
  protected[DFiant] type `U<U`[LW, RW] = UopUeqB[DiSoOp.Kind.<, LW, RW]
  implicit def `evU<U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<U`[LW, RW]]
  protected[DFiant] type `U>U`[LW, RW] = UopUeqB[DiSoOp.Kind.>, LW, RW]
  implicit def `evU>U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>U`[LW, RW]]
  protected[DFiant] type `U<=U`[LW, RW] = UopUeqB[DiSoOp.Kind.<=, LW, RW]
  implicit def `evU<=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<=U`[LW, RW]]
  protected[DFiant] type `U>=U`[LW, RW] = UopUeqB[DiSoOp.Kind.>=, LW, RW]
  implicit def `evU>=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>=U`[LW, RW]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFEnum
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected type EopEeqB[Kind <: DiSoOp.Kind, E <: Enum] = DiSoOp[Kind, DFEnum[E], DFEnum[E], DFBool]
  protected[DFiant] type `E==E`[E <: Enum] = EopEeqB[DiSoOp.Kind.==, E]
  implicit def `evE==E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E==E`[E]]
  protected[DFiant] type `E!=E`[E <: Enum] = EopEeqB[DiSoOp.Kind.!=, E]
  implicit def `evE!=E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E!=E`[E]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


