package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation

trait Series {
  trait RTAddSub[LW, RW, WCW] extends RTComponent {
    val A : DFUInt[LW] <> IN
    val B : DFUInt[RW] <> IN
    val S : DFUInt[WCW] <> OUT
  }

  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`[LW, RW, WCW](implicit dsn : DFDesign) : Implementation[`U+U`[LW, RW, WCW]] = ifc => {
      import ifc._
      new RTAddSub[LW, RW, WCW] {
        val A = inLeft
        val B = inRight
        val S = outResult
      }
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

    implicit def `evE==E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign) : Implementation[`E==E`[LE, RE]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[LE <: Enum#DFEnum, RE <: Enum#DFEnum](implicit dsn : DFDesign) : Implementation[`E!=E`[LE, RE]] = ifc => {
      import ifc._
    }

  }

}

object Series {
  trait `7` extends Series {

  }
}
