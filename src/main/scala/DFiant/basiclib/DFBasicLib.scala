package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {
  implicit val basicLib : DFBasicLib = this

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected type UopUeqU[Kind <: DiSoOp.Kind, LW, RW, WCW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFUInt[WCW]]
  protected type UopUeqB[Kind <: DiSoOp.Kind, LW, RW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFBool]

  protected[DFiant] type `U+U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.+, LW, RW, WCW]
  implicit def `evU+U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U+U`[LW, RW, WCW]]

  protected[DFiant] type `U-U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.-, LW, RW, WCW]
  implicit def `evU-U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U-U`[LW, RW, WCW]]

  protected[DFiant] type `U*U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.*, LW, RW, WCW]
  implicit def `evU*U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U*U`[LW, RW, WCW]]

  protected[DFiant] type `U==U`[LW, RW] = UopUeqB[DiSoOp.Kind.==, LW, RW]
  implicit def `evU==U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U==U`[LW, RW]]
  protected[DFiant] type `U!=U`[LW, RW] = UopUeqB[DiSoOp.Kind.!=, LW, RW]
  implicit def `evU!=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U!=U`[LW, RW]]
  protected[DFiant] type `U<U`[LW, RW] = UopUeqB[DiSoOp.Kind.<, LW, RW]
  implicit def `evU<U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<U`[LW, RW]]
  protected[DFiant] type `U>U`[LW, RW] = UopUeqB[DiSoOp.Kind.>, LW, RW]
  implicit def `evU>U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>U`[LW, RW]]
  protected[DFiant] type `U<=U`[LW, RW] = UopUeqB[DiSoOp.Kind.<=, LW, RW]
  implicit def `evU<=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<=U`[LW, RW]]
  protected[DFiant] type `U>=U`[LW, RW] = UopUeqB[DiSoOp.Kind.>=, LW, RW]
  implicit def `evU>=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>=U`[LW, RW]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFEnum
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected type EopEeqB[Kind <: DiSoOp.Kind, LE <: Enum, RE <: Enum] = DiSoOp[Kind, DFEnum[LE], DFEnum[RE], DFBool]
  protected[DFiant] type `E==E`[LE <: Enum, RE <: Enum] = EopEeqB[DiSoOp.Kind.==, LE, RE]
  implicit def `evE==E`[LE <: Enum, RE <: Enum](implicit dsn : DFDesign) : Implementation[`E==E`[LE, RE]]
  protected[DFiant] type `E!=E`[LE <: Enum, RE <: Enum] = EopEeqB[DiSoOp.Kind.!=, LE, RE]
  implicit def `evE!=E`[LE <: Enum, RE <: Enum](implicit dsn : DFDesign) : Implementation[`E!=E`[LE, RE]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


