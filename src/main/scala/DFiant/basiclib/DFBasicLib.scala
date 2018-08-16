package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps
  val DFBoolOps : DFBasicLib.DFBoolOps


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFEnum
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class EopEeqB[Kind <: DiSoOp.Kind, E <: Enum](e : E)(
    implicit ctx : DFComponent.Context[EopEeqB[Kind, E]]
  ) extends DFComponent[EopEeqB[Kind, E]] {
    final lazy val inLeft = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val inRight = ??? //new DFEnum.NewVar[E]() <> IN
    final lazy val outResult = ??? //DFBool() <> OUT
  }
  type `E==E`[E <: Enum] = EopEeqB[DiSoOp.Kind.==, E]
  implicit def `evE==E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E==E`[E]]
  type `E!=E`[E <: Enum] = EopEeqB[DiSoOp.Kind.!=, E]
  implicit def `evE!=E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E!=E`[E]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


object DFBasicLib {
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    class Arithmetic[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
      implicit ctx : DFComponent.Context[Arithmetic[Kind]], kind : Kind
    ) extends DFComponent[Arithmetic[Kind]] {
      final val inLeft = DFUInt(leftWidth) <> IN
      final val inRight = DFUInt(rightWidth) <> IN
      final val outResult = DFUInt(resultWidth) <> OUT
      kind match {
        case _: DiSoOp.Kind.+ => setInitFunc(outResult)(() => DFUInt.Token.+(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.- => setInitFunc(outResult)(() => DFUInt.Token.-(getInit(inLeft), getInit(inRight)))
        case _: DiSoOp.Kind.* => setInitFunc(outResult)(() => DFUInt.Token.*(getInit(inLeft), getInit(inRight)))
        case _ =>
      }
//      override lazy val typeName: String = s"`basicLib$$DFUIntOps$$Comp$kind`"
    }

    type `Comp+` = Arithmetic[DiSoOp.Kind.+]
    implicit def `ev+`(implicit ctx : Implementation.Context) : Implementation[`Comp+`]
    type `Comp-` = Arithmetic[DiSoOp.Kind.-]
    implicit def `ev-`(implicit ctx : Implementation.Context) : Implementation[`Comp-`]
    type `Comp*` = Arithmetic[DiSoOp.Kind.*]
    implicit def `ev*`(implicit ctx : Implementation.Context) : Implementation[`Comp*`]

    class Relational[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int)(
      implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
    ) extends DFComponent[Relational[Kind]] {
      final val inLeft = DFUInt(leftWidth) <> IN
      final val inRight = DFUInt(rightWidth) <> IN
      final val outResult = DFBool() <> OUT
      override lazy val typeName: String = s"`basicLib$$DFUIntOps$$Comp$kind`"
    }

    type `Comp==` = Relational[DiSoOp.Kind.==]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    type `Comp!=` = Relational[DiSoOp.Kind.!=]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
    type `Comp<` = Relational[DiSoOp.Kind.<]
    implicit def `ev<`(implicit ctx : Implementation.Context) : Implementation[`Comp<`]
    type `Comp>` = Relational[DiSoOp.Kind.>]
    implicit def `ev>`(implicit ctx : Implementation.Context) : Implementation[`Comp>`]
    type `Comp<=` = Relational[DiSoOp.Kind.<=]
    implicit def `ev<=`(implicit ctx : Implementation.Context) : Implementation[`Comp<=`]
    type `Comp>=` = Relational[DiSoOp.Kind.>=]
    implicit def `ev>=`(implicit ctx : Implementation.Context) : Implementation[`Comp>=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBitsOps {
    class Bitwise[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
      implicit ctx : DFComponent.Context[Bitwise[Kind]], kind : Kind
    ) extends DFComponent[Bitwise[Kind]] {
      final val inLeft = DFBits(leftWidth) <> IN
      final val inRight = DFBits(rightWidth) <> IN
      final val outResult = DFBits(resultWidth) <> OUT
      override lazy val typeName: String = s"`basicLib$$DFBitsOps$$Comp$kind`"
    }

    type `Comp|` = Bitwise[DiSoOp.Kind.|]
    implicit def `ev|`(implicit ctx : Implementation.Context) : Implementation[`Comp|`]
    type `Comp&` = Bitwise[DiSoOp.Kind.&]
    implicit def `ev&`(implicit ctx : Implementation.Context) : Implementation[`Comp&`]
    type `Comp^` = Bitwise[DiSoOp.Kind.^]
    implicit def `ev^`(implicit ctx : Implementation.Context) : Implementation[`Comp^`]
//    type `Comp<<` = Bitwise[DiSoOp.Kind.<<] //left-shift by vector
//    implicit def `ev<<`(implicit ctx : Implementation.Context) : Implementation[`Comp<<`]
//    type `Comp>>` = Bitwise[DiSoOp.Kind.>>] //right-shift by vector
//    implicit def `ev>>`(implicit ctx : Implementation.Context) : Implementation[`Comp>>`]

    class Relational[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int)(
      implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
    ) extends DFComponent[Relational[Kind]] {
      final val inLeft = DFBits(leftWidth) <> IN
      final val inRight = DFBits(rightWidth) <> IN
      final val outResult = DFBool() <> OUT
      override lazy val typeName: String = s"`basicLib$$DFBitsOps$$Comp$kind`"
    }

    type `Comp==` = Relational[DiSoOp.Kind.==]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    type `Comp!=` = Relational[DiSoOp.Kind.!=]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBool
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBoolOps {
    class BoolopBool[Kind <: DiSoOp.Kind](
      implicit ctx : DFComponent.Context[BoolopBool[Kind]], kind : Kind
    ) extends DFComponent[BoolopBool[Kind]] {
      final val inLeft = DFBool() <> IN
      final val inRight = DFBool() <> IN
      final val outResult = DFBool() <> OUT
      override lazy val typeName: String = s"`basicLib$$DFBoolOps$$Comp$kind`"
    }

    type `Comp||` = BoolopBool[DiSoOp.Kind.||]
    implicit def `ev||`(implicit ctx : Implementation.Context) : Implementation[`Comp||`]
    type `Comp&&` = BoolopBool[DiSoOp.Kind.&&]
    implicit def `ev&&`(implicit ctx : Implementation.Context) : Implementation[`Comp&&`]
    type `Comp==` = BoolopBool[DiSoOp.Kind.==]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    type `Comp!=` = BoolopBool[DiSoOp.Kind.!=]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}