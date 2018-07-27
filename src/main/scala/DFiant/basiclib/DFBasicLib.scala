package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
trait DFBasicLib {
  implicit val basicLib : DFBasicLib = this

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class UopUeqU[Kind <: DiSoOp.Kind](val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[UopUeqU[Kind]]
  ) extends DFComponent[UopUeqU[Kind]] {
    final lazy val inLeft = DFUInt(leftWidth) <> IN
    final lazy val inRight = DFUInt(rightWidth) <> IN
    final lazy val outResult = DFUInt(resultWidth) <> OUT
  }

  protected[DFiant] type `U+U` = UopUeqU[DiSoOp.Kind.+]
  implicit def `evU+U`(implicit ctx : DFAny.Op.Context) : Implementation[`U+U`]
  protected[DFiant] type `U-U` = UopUeqU[DiSoOp.Kind.-]
  implicit def `evU-U`(implicit ctx : DFAny.Op.Context) : Implementation[`U-U`]
  protected[DFiant] type `U*U` = UopUeqU[DiSoOp.Kind.*]
  implicit def `evU*U`(implicit ctx : DFAny.Op.Context) : Implementation[`U*U`]

  class UopUeqB[Kind <: DiSoOp.Kind](leftWidth : Int, rightWidth : Int)(
    implicit ctx : DFComponent.Context[UopUeqB[Kind]]
  ) extends DFComponent[UopUeqB[Kind]] {
    final lazy val inLeft = DFUInt(leftWidth) <> IN
    final lazy val inRight = DFUInt(rightWidth) <> IN
    final lazy val outResult = DFBool() <> OUT
  }

  protected[DFiant] type `U==U` = UopUeqB[DiSoOp.Kind.==]
  implicit def `evU==U`(implicit ctx : DFAny.Op.Context) : Implementation[`U==U`]
  protected[DFiant] type `U!=U` = UopUeqB[DiSoOp.Kind.!=]
  implicit def `evU!=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U!=U`]
  protected[DFiant] type `U<U` = UopUeqB[DiSoOp.Kind.<]
  implicit def `evU<U`(implicit ctx : DFAny.Op.Context) : Implementation[`U<U`]
  protected[DFiant] type `U>U` = UopUeqB[DiSoOp.Kind.>]
  implicit def `evU>U`(implicit ctx : DFAny.Op.Context) : Implementation[`U>U`]
  protected[DFiant] type `U<=U` = UopUeqB[DiSoOp.Kind.<=]
  implicit def `evU<=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U<=U`]
  protected[DFiant] type `U>=U` = UopUeqB[DiSoOp.Kind.>=]
  implicit def `evU>=U`(implicit ctx : DFAny.Op.Context) : Implementation[`U>=U`]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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
  protected[DFiant] type `E==E`[E <: Enum] = EopEeqB[DiSoOp.Kind.==, E]
  implicit def `evE==E`[E <: Enum](implicit ctx : DFAny.Op.Context) : Implementation[`E==E`[E]]
  protected[DFiant] type `E!=E`[E <: Enum] = EopEeqB[DiSoOp.Kind.!=, E]
  implicit def `evE!=E`[E <: Enum](implicit ctx : DFAny.Op.Context) : Implementation[`E!=E`[E]]
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


