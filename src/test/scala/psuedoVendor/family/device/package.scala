package psuedoVendor.family

import DFiant._
import DFComponent.Implementation
package object device {
  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `ev+`[LW, RW, WCW] : Implementation[`U+U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }
    implicit def `ev-`[LW, RW, WCW] : Implementation[`U-U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }
    implicit def `ev*`[LW, RW, WCW] : Implementation[`U*U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }
  }
}
