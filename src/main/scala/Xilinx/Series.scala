package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation

trait Series {
  trait RTAddSub extends RTComponent {
//    protected val aWidth : Int
//    protected val bWidth : Int
//    protected val sWidth : Int
    val A : DFUInt.Unbounded <> IN
    val B : DFUInt.Unbounded <> IN
    val S : DFUInt.Unbounded <> OUT
//    final lazy val A = DFUInt(aWidth) <> IN
//    final lazy val B = DFUInt(bWidth) <> IN
//    final lazy val S = DFUInt(sWidth) <> OUT
  }

  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`(implicit dsn : DFDesign) : Implementation[`U+U`] = ifc => {
      import ifc._
//      val rtInst = new RTAddSub {
////        protected val aWidth : Int = inLeft.width
////        protected val bWidth : Int = inRight.width
////        protected val sWidth : Int = outResult.width
//        val A = inLeft
//        val B = inRight
//        val S = outResult
//      }

    }
    implicit def `evU-U`(implicit dsn : DFDesign) : Implementation[`U-U`] = ifc => {
      import ifc._
    }
    implicit def `evU*U`(implicit dsn : DFDesign) : Implementation[`U*U`] = ifc => {
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

    implicit def `evE==E`[E <: Enum](implicit dsn : DFDesign) : Implementation[`E==E`[E]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[E <: Enum](implicit dsn : DFDesign) : Implementation[`E!=E`[E]] = ifc => {
      import ifc._
    }

  }

}

object Series {
  trait `7` extends Series {

  }
}
