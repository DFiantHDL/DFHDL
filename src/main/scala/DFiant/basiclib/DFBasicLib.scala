package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {

  implicit def `ev+`[LW, RW, ResW] : Implementation[DiSoOp[DiSoOp.Kind.+, DFUInt[LW], DFUInt[RW], DFUInt[ResW]]] = ifc => {
    import ifc._
  }


}

object DFBasicLib extends DFBasicLib
