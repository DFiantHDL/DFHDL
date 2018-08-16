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
    import DFiant.basiclib.DFUIntOps._
    implicit def `ev+`(implicit ctx : Implementation.Context) : Implementation[`Comp+`]
    implicit def `ev-`(implicit ctx : Implementation.Context) : Implementation[`Comp-`]
    implicit def `ev*`(implicit ctx : Implementation.Context) : Implementation[`Comp*`]

    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
    implicit def `ev<`(implicit ctx : Implementation.Context) : Implementation[`Comp<`]
    implicit def `ev>`(implicit ctx : Implementation.Context) : Implementation[`Comp>`]
    implicit def `ev<=`(implicit ctx : Implementation.Context) : Implementation[`Comp<=`]
    implicit def `ev>=`(implicit ctx : Implementation.Context) : Implementation[`Comp>=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBitsOps {
    import DFiant.basiclib.DFBitsOps._
    implicit def `ev|`(implicit ctx : Implementation.Context) : Implementation[`Comp|`]
    implicit def `ev&`(implicit ctx : Implementation.Context) : Implementation[`Comp&`]
    implicit def `ev^`(implicit ctx : Implementation.Context) : Implementation[`Comp^`]

    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBool
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBoolOps {
    import DFiant.basiclib.DFBoolOps._
    implicit def `ev||`(implicit ctx : Implementation.Context) : Implementation[`Comp||`]
    implicit def `ev&&`(implicit ctx : Implementation.Context) : Implementation[`Comp&&`]
    implicit def `ev==`(implicit ctx : Implementation.Context) : Implementation[`Comp==`]
    implicit def `ev!=`(implicit ctx : Implementation.Context) : Implementation[`Comp!=`]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}