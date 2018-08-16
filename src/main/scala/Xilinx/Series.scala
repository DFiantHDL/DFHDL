package Xilinx
import DFiant._
import DFiant.DFComponent.Implementation
import DFiant.basiclib.DFBasicLib

trait Series {
  implicit object basicLib extends DFiant.basiclib.DFBasicLib {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFUIntOps extends DFBasicLib.DFUIntOps {
      import DFiant.basiclib.DFUIntOps._
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

      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (initFunc : (Seq[DFUInt.Token], Seq[DFUInt.Token]) => Seq[DFBool.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFBool() <> OUT
        setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
      }

      implicit def `ev+`(implicit ctx : Implementation.Context) : Implementation[`Comp+`] = ifc => {
        import ifc._
        val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)(DFUInt.Token.+)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev-`(implicit ctx : Implementation.Context) : Implementation[`Comp-`] = ifc => {
        import ifc._
        val rtInst = new RTAddSub(leftWidth, rightWidth, resultWidth)(DFUInt.Token.-)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev*`(implicit ctx : Implementation.Context) : Implementation[`Comp*`] = ifc => {
        import ifc._
        val rtInst = new RTMul(leftWidth, rightWidth, resultWidth)(DFUInt.Token.*)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }

      implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)(DFUInt.Token.==)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)(DFUInt.Token.!=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev<`(implicit ctx : Implementation.Context) : Implementation[`Comp<`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("<")(leftWidth, rightWidth)(DFUInt.Token.<)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev>`(implicit ctx : Implementation.Context) : Implementation[`Comp>`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp(">")(leftWidth, rightWidth)(DFUInt.Token.>)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev<=`(implicit ctx : Implementation.Context) : Implementation[`Comp<=`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("<=")(leftWidth, rightWidth)(DFUInt.Token.<=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev>=`(implicit ctx : Implementation.Context) : Implementation[`Comp>=`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp(">=")(leftWidth, rightWidth)(DFUInt.Token.>=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBitsOps extends DFBasicLib.DFBitsOps {
      import DFiant.basiclib.DFBitsOps._
      class RTInfixBitwiseOp(opString : String)(aWidth : Int, bWidth : Int, sWidth : Int)
        (initFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBits.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBits(sWidth) <> OUT
        setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
      }
      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (initFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBool.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBool() <> OUT
        setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
      }
      implicit def `ev|`(implicit ctx : Implementation.Context) : Implementation[`Comp|`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBitwiseOp("|")(leftWidth, rightWidth, resultWidth)(DFBits.Token.|)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev&`(implicit ctx : Implementation.Context) : Implementation[`Comp&`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBitwiseOp("&")(leftWidth, rightWidth, resultWidth)(DFBits.Token.&)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev^`(implicit ctx : Implementation.Context) : Implementation[`Comp^`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBitwiseOp("^")(leftWidth, rightWidth, resultWidth)(DFBits.Token.^)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)(DFBits.Token.==)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`] = ifc => {
        import ifc._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)(DFBits.Token.!=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    implicit def `evE==E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E==E`[E]] = ifc => {
      import ifc._
    }
    implicit def `evE!=E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E!=E`[E]] = ifc => {
      import ifc._
    }
    
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBool
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBoolOps extends DFBasicLib.DFBoolOps {
      import DFiant.basiclib.DFBoolOps._
      class RTInfixBoolOp(opString : String)
        (initFunc : (Seq[DFBool.Token], Seq[DFBool.Token]) => Seq[DFBool.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBool() <> IN
        final val B = DFBool() <> IN
        final val S = DFBool() <> OUT
        setInitFunc(S)(() => initFunc(getInit(A), getInit(B)))
      }
      implicit def `ev||`(implicit ctx : Implementation.Context) : Implementation[`Comp||`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBoolOp("||")(DFBool.Token.||)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev&&`(implicit ctx : Implementation.Context) : Implementation[`Comp&&`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBoolOp("&&")(DFBool.Token.&&)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBoolOp("==")(DFBool.Token.==)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`] = ifc => {
        import ifc._
        val rtInst = new RTInfixBoolOp("!=")(DFBool.Token.!=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }

}

object Series {
  trait `7` extends Series {

  }
}
