/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package Xilinx
import DFiant._
import DFiant.BasicLib.DFBasicLib
import internals._

trait Series {
  implicit object basicLib extends DFiant.BasicLib.DFBasicLib {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFUIntOps extends DFBasicLib.DFUIntOps {
      import DFiant.BasicLib.DFUIntOps._
      class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
      }

      class RTSub(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.-, getInit(A), getInit(B)))
      }

      class RTMul(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.*, getInit(A), getInit(B)))
      }

      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFBool() <> OUT
        opString match {
          case "==" => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.==, getInit(A), getInit(B)))
          case "!=" => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.!=, getInit(A), getInit(B)))
          case "<"  => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.<, getInit(A), getInit(B)))
          case ">"  => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.>, getInit(A), getInit(B)))
          case "<=" => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.<=, getInit(A), getInit(B)))
          case ">=" => setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.>=, getInit(A), getInit(B)))
        }
      }

      implicit val `Comp+` : `Comp+` => Unit = comp => {
        import comp._
        val rtInst = new RTAdd(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp-` : `Comp-` => Unit = comp => {
        import comp._
        val rtInst = new RTSub(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp*` : `Comp*` => Unit = comp => {
        import comp._
        val rtInst = new RTMul(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }

      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp<` : `Comp<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("<")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>` : `Comp>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp(">")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp<=` : `Comp<=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("<=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>=` : `Comp>=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp(">=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFSInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFSIntOps extends DFBasicLib.DFSIntOps {
      import DFiant.BasicLib.DFSIntOps._
      class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.+, getInit(A), getInit(B)))
      }

      class RTSub(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.-, getInit(A), getInit(B)))
      }

      class RTMul(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.*, getInit(A), getInit(B)))
      }

      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFBool() <> OUT
        opString match {
          case "==" => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.==, getInit(A), getInit(B)))
          case "!=" => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.!=, getInit(A), getInit(B)))
          case "<"  => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.<, getInit(A), getInit(B)))
          case ">"  => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.>, getInit(A), getInit(B)))
          case "<=" => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.<=, getInit(A), getInit(B)))
          case ">=" => setInitFunc(S)(LazyBox.Args2(this)(DFSInt.Token.>=, getInit(A), getInit(B)))
        }
      }

      implicit val `Comp+` : `Comp+` => Unit = comp => {
        import comp._
        val rtInst = new RTAdd(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp-` : `Comp-` => Unit = comp => {
        import comp._
        val rtInst = new RTSub(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp*` : `Comp*` => Unit = comp => {
        import comp._
        val rtInst = new RTMul(leftWidth, rightWidth, resultWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }

      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp<` : `Comp<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("<")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>` : `Comp>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp(">")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp<=` : `Comp<=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("<=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>=` : `Comp>=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp(">=")(leftWidth, rightWidth)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      class RTInfixShiftOp(opString : String)(aWidth : Int, bWidth : Int)
        (initFunc : (Seq[DFSInt.Token], Seq[DFUInt.Token]) => Seq[DFSInt.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFSInt(aWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(initFunc, getInit(A), getInit(B)))
      }
      implicit val `Comp<<` : `Comp<<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp("<<")(leftWidth, rightWidth)(DFSInt.Token.<<)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>>` : `Comp>>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp(">>")(leftWidth, rightWidth)(DFSInt.Token.>>)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBits
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBitsOps extends DFBasicLib.DFBitsOps {
      import DFiant.BasicLib.DFBitsOps._
      class RTInfixBitwiseOp(opString : String)(aWidth : Int, bWidth : Int, sWidth : Int)
        (initFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBits.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBits(sWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(initFunc, getInit(A), getInit(B)))
      }
      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (initFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBool.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBool() <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(initFunc, getInit(A), getInit(B)))
      }
      implicit val `Comp|` : `Comp|` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("|")(leftWidth, rightWidth, resultWidth)(DFBits.Token.|)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp&` : `Comp&` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("&")(leftWidth, rightWidth, resultWidth)(DFBits.Token.&)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp^` : `Comp^` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("^")(leftWidth, rightWidth, resultWidth)(DFBits.Token.^)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)(DFBits.Token.==)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)(DFBits.Token.!=)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      class RTInfixShiftOp(opString : String)(aWidth : Int, bWidth : Int)
        (initFunc : (Seq[DFBits.Token], Seq[DFUInt.Token]) => Seq[DFBits.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFBits(aWidth) <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(initFunc, getInit(A), getInit(B)))
      }
      implicit val `Comp<<` : `Comp<<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp("<<")(leftWidth, rightWidth)(DFBits.Token.<<)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>>` : `Comp>>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp(">>")(leftWidth, rightWidth)(DFBits.Token.>>)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBool
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBoolOps extends DFBasicLib.DFBoolOps {
      import DFiant.BasicLib.DFBoolOps._
      class RTInfixBoolOp(opString : String)
        (initFunc : (Seq[DFBool.Token], Seq[DFBool.Token]) => Seq[DFBool.Token])
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBool() <> IN
        final val B = DFBool() <> IN
        final val S = DFBool() <> OUT
        setInitFunc(S)(LazyBox.Args2(this)(initFunc, getInit(A), getInit(B)))
      }
      implicit val `Comp||` : `Comp||` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("||")(DFBool.Token.||)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp&&` : `Comp&&` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("&&")(DFBool.Token.&&)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp^`  : `Comp^` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("^")(DFBool.Token.^)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("==")(DFBool.Token.==)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
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
