package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation
import DFiant.basiclib.DFBasicLib

trait Series {
  class RTAddSub(aWidth : Int, bWidth : Int, sWidth : Int)
    (initFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFUInt.Token])
    (implicit ctx : RTComponent.Context) extends RTComponent {
    final val A = DFUInt(aWidth) <> IN
    final val B = DFUInt(bWidth) <> IN
    final val S = DFUInt(sWidth) <> OUT
    setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
  }

  class RTMul(aWidth : Int, bWidth : Int, sWidth : Int)
    (initFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFUInt.Token])
    (implicit ctx : RTComponent.Context) extends RTComponent {
    final val A = DFUInt(aWidth) <> IN
    final val B = DFUInt(bWidth) <> IN
    final val S = DFUInt(sWidth) <> OUT
    setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
  }

  class RTInfixCompareOp(opString : String)(aWidth : Int, bWidth : Int)
    (initFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFBool.Token])
    (implicit ctx : RTComponent.Context) extends RTComponent {
    final val A = DFUInt(aWidth) <> IN
    final val B = DFUInt(bWidth) <> IN
    final val S = DFBool() <> OUT
    setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
  }

  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    implicit def `evU+U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U+U`[LW, RW, OW]] = ifc => {
      import ifc._
      val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)(DFUInt.Token.+)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU-U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U-U`[LW, RW, OW]] = ifc => {
      import ifc._
      val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)(DFUInt.Token.-)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU*U`[LW, RW, OW](implicit ctx : Implementation.Context) : Implementation[`U*U`[LW, RW, OW]] = ifc => {
      import ifc._
      val rtInst = new RTMul(leftWidth, rightWidth, resultWidth)(DFUInt.Token.*)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }

    implicit def `evU==U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U==U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp("==")(leftWidth, rightWidth)(DFUInt.Token.==)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU!=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U!=U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp("!=")(leftWidth, rightWidth)(DFUInt.Token.!=)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU<U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U<U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp("<")(leftWidth, rightWidth)(DFUInt.Token.<)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU>U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U>U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp(">")(leftWidth, rightWidth)(DFUInt.Token.>)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU<=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U<=U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp("<=")(leftWidth, rightWidth)(DFUInt.Token.<=)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
    }
    implicit def `evU>=U`[LW, RW](implicit ctx : Implementation.Context) : Implementation[`U>=U`[LW, RW]] = ifc => {
      import ifc._
      val rtInst = new RTInfixCompareOp(">=")(leftWidth, rightWidth)(DFUInt.Token.>=)
      rtInst.A <> inLeft
      rtInst.B <> inRight
      rtInst.S <> outResult
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
