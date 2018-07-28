package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation

trait Series {
  class RTAddSub(aWidth : Int, bWidth : Int, sWidth : Int)(implicit ctx : DFAny.Op.Context) extends RTComponent {
    final val A = DFUInt(aWidth) <> IN
    final val B = DFUInt(bWidth) <> IN
    final val S = DFUInt(sWidth) <> OUT
  }

  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`(implicit ctx : DFAny.Op.Context) : Implementation[`U+U`] = ifc => {
      import ifc._
      val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
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

object Series {
  trait `7` extends Series {

  }
}
