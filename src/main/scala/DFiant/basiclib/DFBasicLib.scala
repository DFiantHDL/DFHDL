package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._
sealed trait AllowUnchecked

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps


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
  protected implicit object AllowUnchecked extends AllowUnchecked

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    class Arithmetic[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
      implicit ctx : DFComponent.Context[Arithmetic[Kind]]
    ) extends DFComponent[Arithmetic[Kind]] {
      final val inLeft = DFUInt.unchecked(leftWidth) <> IN
      final val inRight = DFUInt.unchecked(rightWidth) <> IN
      final val outResult = DFUInt.unchecked(resultWidth) <> OUT
    }

    type `Comp+` = Arithmetic[DiSoOp.Kind.+]
    implicit def `ev+`(implicit ctx : Implementation.Context) : Implementation[`Comp+`]
    type `Comp-` = Arithmetic[DiSoOp.Kind.-]
    implicit def `ev-`(implicit ctx : Implementation.Context) : Implementation[`Comp-`]
    type `Comp*` = Arithmetic[DiSoOp.Kind.*]
    implicit def `ev*`(implicit ctx : Implementation.Context) : Implementation[`Comp*`]

    class Relational[Kind <: DiSoOp.Kind](
      val leftWidth : Int, val rightWidth : Int)(
      implicit ctx : DFComponent.Context[Relational[Kind]]
    ) extends DFComponent[Relational[Kind]] {
      final val inLeft = DFUInt.unchecked(leftWidth) <> IN
      final val inRight = DFUInt.unchecked(rightWidth) <> IN
      final val outResult = DFBool() <> OUT
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
      implicit ctx : DFComponent.Context[Bitwise[Kind]]
    ) extends DFComponent[Bitwise[Kind]] {
      final val inLeft = DFBits.unchecked(leftWidth) <> IN
      final val inRight = DFBits.unchecked(rightWidth) <> IN
      final val outResult = DFBits.unchecked(resultWidth) <> OUT
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
      implicit ctx : DFComponent.Context[Relational[Kind]]
    ) extends DFComponent[Relational[Kind]] {
      final val inLeft = DFBits.unchecked(leftWidth) <> IN
      final val inRight = DFBits.unchecked(rightWidth) <> IN
      final val outResult = DFBool() <> OUT
    }

    type `Comp==` = Relational[DiSoOp.Kind.==]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    type `Comp!=` = Relational[DiSoOp.Kind.!=]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}