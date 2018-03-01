package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {
//  implicit val protBasicLib : DFBasicLib = this

  protected type UopUeqU[Kind <: DiSoOp.Kind, LW, RW, WCW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFUInt[WCW]]
  protected type UopUeqB[Kind <: DiSoOp.Kind, LW, RW] = DiSoOp[Kind, DFUInt[LW], DFUInt[RW], DFBool]

  protected[DFiant] type `U+U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.+, LW, RW, WCW]
  implicit def `ev+`[LW, RW, WCW] : Implementation[`U+U`[LW, RW, WCW]]

  protected[DFiant] type `U-U`[LW, RW, WCW] = UopUeqU[DiSoOp.Kind.-, LW, RW, WCW]
  implicit def `ev-`[LW, RW, WCW] : Implementation[`U-U`[LW, RW, WCW]]
}

object DFBasicLib extends DFBasicLib {
  implicit def `ev+`[LW, RW, WCW] : Implementation[`U+U`[LW, RW, WCW]] = ifc => {}
  implicit def `ev-`[LW, RW, WCW] : Implementation[`U-U`[LW, RW, WCW]] = ifc => {}
}
