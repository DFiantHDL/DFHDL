package psuedoVendor.family

import DFiant._
import DFComponent.Implementation
package object device {
  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`(implicit blk : DFBlock) : Implementation[`U+U`] = ifc => {
      import ifc._
    }
    implicit def `evU-U`(implicit blk : DFBlock) : Implementation[`U-U`] = ifc => {
      import ifc._
    }
    implicit def `evU*U`(implicit blk : DFBlock) : Implementation[`U*U`] = ifc => {
      import ifc._
    }

    implicit def `evU==U`[LW, RW](implicit blk : DFBlock) : Implementation[`U==U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU!=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U!=U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU<U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU>U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU<=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<=U`[LW, RW]] = ifc => {
      import ifc._
    }
    implicit def `evU>=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>=U`[LW, RW]] = ifc => {
      import ifc._
    }

    implicit def `evE==E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E==E`[E]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E!=E`[E]] = ifc => {
      import ifc._
    }

  }
}
