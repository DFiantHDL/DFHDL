package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation

trait Series {
  class RTAddSub(aWidth : Int, bWidth : Int, sWidth : Int)
    (initFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFUInt.Token])
    (implicit ctx : RTComponent.Context) extends RTComponent {
    final val A = DFUInt(aWidth) <> IN
    final val B = DFUInt(bWidth) <> IN
    final val S = DFUInt(sWidth) <> OUT
    setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
  }

  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`(implicit ctx : Implementation.Context) : Implementation[`U+U`] = ifc => {
      import ifc._
      val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)(DFUInt.Token.+)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU-U`(implicit ctx : Implementation.Context) : Implementation[`U-U`] = ifc => {
      import ifc._
    }
    implicit def `evU*U`(implicit ctx : Implementation.Context) : Implementation[`U*U`] = ifc => {
      import ifc._
    }

    implicit def `evU==U`(implicit ctx : Implementation.Context) : Implementation[`U==U`] = ifc => {
      import ifc._
    }
    implicit def `evU!=U`(implicit ctx : Implementation.Context) : Implementation[`U!=U`] = ifc => {
      import ifc._
    }
    implicit def `evU<U`(implicit ctx : Implementation.Context) : Implementation[`U<U`] = ifc => {
      import ifc._
    }
    implicit def `evU>U`(implicit ctx : Implementation.Context) : Implementation[`U>U`] = ifc => {
      import ifc._
    }
    implicit def `evU<=U`(implicit ctx : Implementation.Context) : Implementation[`U<=U`] = ifc => {
      import ifc._
    }
    implicit def `evU>=U`(implicit ctx : Implementation.Context) : Implementation[`U>=U`] = ifc => {
      import ifc._
    }

    implicit def `evE==E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E==E`[E]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E!=E`[E]] = ifc => {
      import ifc._
    }

  }

}

object Series {
  trait `7` extends Series {

  }
}
