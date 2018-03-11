package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {
  implicit val basicLib : DFBasicLib = this

  protected type UopUeqU[Kind <: DiSoOp.Kind, LW, RW, WCW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFUInt[WCW]]
  protected type UopUeqB[Kind <: DiSoOp.Kind, LW, RW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFBool]

  protected[DFiant] type `U+U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.+, LW, RW, WCW]
  implicit def `ev+`[LW, RW, WCW] : Implementation[`U+U`[LW, RW, WCW]]

  protected[DFiant] type `U-U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.-, LW, RW, WCW]
  implicit def `ev-`[LW, RW, WCW] : Implementation[`U-U`[LW, RW, WCW]]

  protected[DFiant] type `U*U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.*, LW, RW, WCW]
  implicit def `ev*`[LW, RW, WCW] : Implementation[`U*U`[LW, RW, WCW]]

  protected[DFiant] type `U==U`[LW, RW] = UopUeqB[DiSoOp.Kind.==, LW, RW]
  implicit def `ev==`[LW, RW] : Implementation[`U==U`[LW, RW]]
  protected[DFiant] type `U!=U`[LW, RW] = UopUeqB[DiSoOp.Kind.!=, LW, RW]
  implicit def `ev!=`[LW, RW] : Implementation[`U!=U`[LW, RW]]
  protected[DFiant] type `U<U`[LW, RW] = UopUeqB[DiSoOp.Kind.<, LW, RW]
  implicit def `ev<`[LW, RW] : Implementation[`U<U`[LW, RW]]
  protected[DFiant] type `U>U`[LW, RW] = UopUeqB[DiSoOp.Kind.>, LW, RW]
  implicit def `ev>`[LW, RW] : Implementation[`U>U`[LW, RW]]
  protected[DFiant] type `U<=U`[LW, RW] = UopUeqB[DiSoOp.Kind.<=, LW, RW]
  implicit def `ev<=`[LW, RW] : Implementation[`U<=U`[LW, RW]]
  protected[DFiant] type `U>=U`[LW, RW] = UopUeqB[DiSoOp.Kind.>=, LW, RW]
  implicit def `ev>=`[LW, RW] : Implementation[`U>=U`[LW, RW]]

}


