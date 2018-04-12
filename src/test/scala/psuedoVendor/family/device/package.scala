package psuedoVendor.family

import DFiant._
import DFComponent.Implementation
package object device {
  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U+U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }
    implicit def `evU-U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U-U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }
    implicit def `evU*U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U*U`[LW, RW, WCW]] = ifc => {
      import ifc._
    }

    implicit def `evU==U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U==U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU!=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U!=U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU<U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU>U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU<=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U<=U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU>=U`[LW, RW](implicit dsn : DFDesign) : Implementation[`U>=U`[LW, RW]] = ifc => {
      import ifc._
    }

    implicit def `evE==E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign)
    : Implementation[`E==E`[LE, RE]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign)
    : Implementation[`E!=E`[LE, RE]] = ifc => {
      import ifc._
    }
  }
}
