package DFiant.basiclib

import DFiant._
import DFComponent.Implementation
import singleton.twoface._

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps
  val DFBoolOps : DFBasicLib.DFBoolOps
  val DFEnumOps : DFBasicLib.DFEnumOps

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


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFEnum
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFEnumOps {
    import DFiant.basiclib.DFEnumOps._
    implicit def `ev==`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`Comp==`[E]]
    implicit def `ev!=`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`Comp!=`[E]]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}