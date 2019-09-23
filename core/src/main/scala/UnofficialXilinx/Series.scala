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

package UnofficialXilinx
import DFiant._
import DFiant.targetlib.TargetLib
import internals._

trait Series {
  implicit object targetLib extends DFiant.targetlib.TargetLib {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFUIntOps extends TargetLib.DFUIntOps {
      import DFiant.targetlib.DFUIntOps._
      class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l + r))
      }

      class RTSub(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l - r))
      }

      class RTMul(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFUInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l * r))
      }

      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFUInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFBool() <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B){
          opString match {
            case "==" => (l, r) => l == r
            case "!=" => (l, r) => l != r
            case "<"  => (l, r) => l <  r
            case ">"  => (l, r) => l >  r
            case "<=" => (l, r) => l <= r
            case ">=" => (l, r) => l >= r
          }
        })
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
    object DFSIntOps extends TargetLib.DFSIntOps {
      import DFiant.targetlib.DFSIntOps._
      class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l + r))
      }

      class RTSub(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l - r))
      }

      class RTMul(aWidth : Int, bWidth : Int, sWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFSInt(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)((l, r) => l * r))
      }

      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFSInt(bWidth) <> IN
        final val S = DFBool() <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B){
          opString match {
            case "==" => (l, r) => l == r
            case "!=" => (l, r) => l != r
            case "<"  => (l, r) => l <  r
            case ">"  => (l, r) => l >  r
            case "<=" => (l, r) => l <= r
            case ">=" => (l, r) => l >= r
          }
        })
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
        (func : (DFSInt.Token, DFUInt.Token) => DFSInt.Token)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFSInt(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFSInt(aWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)(func))
      }
      implicit val `Comp<<` : `Comp<<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp("<<")(leftWidth, rightWidth)((l, r) => l << r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>>` : `Comp>>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp(">>")(leftWidth, rightWidth)((l, r) => l >> r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBits
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBitsOps extends TargetLib.DFBitsOps {
      import DFiant.targetlib.DFBitsOps._
      class RTInfixBitwiseOp(opString : String)(aWidth : Int, bWidth : Int, sWidth : Int)
        (func : (DFBits.Token, DFBits.Token) => DFBits.Token)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBits(sWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)(func))
      }
      class RTInfixRelationalOp(opString : String)(aWidth : Int, bWidth : Int)
        (func : (DFBits.Token, DFBits.Token) => DFBool.Token)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFBits(bWidth) <> IN
        final val S = DFBool() <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)(func))
      }
      implicit val `Comp|` : `Comp|` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("|")(leftWidth, rightWidth, resultWidth)((l, r) => l | r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp&` : `Comp&` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("&")(leftWidth, rightWidth, resultWidth)((l, r) => l & r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp^` : `Comp^` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBitwiseOp("^")(leftWidth, rightWidth, resultWidth)((l, r) => l ^ r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("==")(leftWidth, rightWidth)((l, r) => l == r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixRelationalOp("!=")(leftWidth, rightWidth)((l, r) => l != r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      class RTInfixShiftOp(opString : String)(aWidth : Int, bWidth : Int)
        (func : (DFBits.Token, DFUInt.Token) => DFBits.Token)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBits(aWidth) <> IN
        final val B = DFUInt(bWidth) <> IN
        final val S = DFBits(aWidth) <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)(func))
      }
      implicit val `Comp<<` : `Comp<<` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp("<<")(leftWidth, rightWidth)((l, r) => l << r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp>>` : `Comp>>` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixShiftOp(">>")(leftWidth, rightWidth)((l, r) => l >> r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
    }
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBool
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    object DFBoolOps extends TargetLib.DFBoolOps {
      import DFiant.targetlib.DFBoolOps._
      class RTInfixBoolOp(opString : String)
        (func : (DFBool.Token, DFBool.Token) => DFBool.Token)
        (implicit ctx : RTComponent.Context) extends RTComponent {
        final val A = DFBool() <> IN
        final val B = DFBool() <> IN
        final val S = DFBool() <> OUT
        final override protected val blackBoxFunctions = Map(S -> BlackBoxFunction(S)(A, B)(func))
      }
      implicit val `Comp||` : `Comp||` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("||")((l, r) => l || r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp&&` : `Comp&&` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("&&")((l, r) => l && r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp^`  : `Comp^` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("^")((l, r) => l ^ r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp==` : `Comp==` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("==")((l, r) => l == r)
        rtInst.A <> inLeft
        rtInst.B <> inRight
        rtInst.S <> outResult
      }
      implicit val `Comp!=` : `Comp!=` => Unit = comp => {
        import comp._
        val rtInst = new RTInfixBoolOp("!=")((l, r) => l != r)
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
