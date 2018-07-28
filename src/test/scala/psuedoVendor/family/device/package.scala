package psuedoVendor.family

import DFiant._
import DFComponent.Implementation
package object device {
  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`(implicit ctx : DFAny.Op.Context) : Implementation[`U+U`] = ifc => {
      import ifc._
    }
    implicit def `evU-U`(implicit ctx : DFAny.Op.Context) : Implementation[`U-U`] = ifc => {
      import ifc._
    }
    implicit def `evU*U`(implicit ctx : DFAny.Op.Context) : Implementation[`U*U`] = ifc => {
      import ifc._
    }

    implicit def `evU==U`(implicit ctx : DFAny.Op.Context) : Implementation[`U==U`] = ifc => {
      import ifc._
    }
    implicit def `evU!=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U!=U`] = ifc => {
      import ifc._
    }
    implicit def `evU<U`(implicit ctx : DFAny.Op.Context) : Implementation[`U<U`] = ifc => {
      import ifc._
    }
    implicit def `evU>U`(implicit ctx : DFAny.Op.Context) : Implementation[`U>U`] = ifc => {
      import ifc._
    }
    implicit def `evU<=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U<=U`] = ifc => {
      import ifc._
    }
    implicit def `evU>=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U>=U`] = ifc => {
      import ifc._
    }

    implicit def `evE==E`[E <: Enum](implicit ctx : DFAny.Op.Context) : Implementation[`E==E`[E]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[E <: Enum](implicit ctx : DFAny.Op.Context) : Implementation[`E!=E`[E]] = ifc => {
      import ifc._
    }

  }
}
