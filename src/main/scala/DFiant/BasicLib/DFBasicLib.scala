package DFiant.BasicLib

trait DFBasicLib {

  val DFUIntOps : DFBasicLib.DFUIntOps
  val DFSIntOps : DFBasicLib.DFSIntOps
  val DFBitsOps : DFBasicLib.DFBitsOps
  val DFBoolOps : DFBasicLib.DFBoolOps

}


object DFBasicLib {
  implicit val default : DFBasicLib = Xilinx.FPGAs.`XC7VX485T-2FFG1761C`.basicLib
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFUInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFUIntOps {
    import DFiant.BasicLib.DFUIntOps._
    implicit val `Comp+`  : `Comp+` => Unit
    implicit val `Comp-`  : `Comp-` => Unit
    implicit val `Comp*`  : `Comp*` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
    implicit val `Comp<`  : `Comp<` => Unit
    implicit val `Comp>`  : `Comp>` => Unit
    implicit val `Comp<=` : `Comp<=` => Unit
    implicit val `Comp>=` : `Comp>=` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFSInt
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFSIntOps {
    import DFiant.BasicLib.DFSIntOps._
    implicit val `Comp+`  : `Comp+` => Unit
    implicit val `Comp-`  : `Comp-` => Unit
    implicit val `Comp*`  : `Comp*` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
    implicit val `Comp<`  : `Comp<` => Unit
    implicit val `Comp>`  : `Comp>` => Unit
    implicit val `Comp<=` : `Comp<=` => Unit
    implicit val `Comp>=` : `Comp>=` => Unit

    implicit val `Comp<<` : `Comp<<` => Unit
    implicit val `Comp>>` : `Comp>>` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBitsOps {
    import DFiant.BasicLib.DFBitsOps._
    implicit val `Comp|`  : `Comp|` => Unit
    implicit val `Comp&`  : `Comp&` => Unit
    implicit val `Comp^`  : `Comp^` => Unit

    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit

    implicit val `Comp<<` : `Comp<<` => Unit
    implicit val `Comp>>` : `Comp>>` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DFBool
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait DFBoolOps {
    import DFiant.BasicLib.DFBoolOps._
    implicit val `Comp||` : `Comp||` => Unit
    implicit val `Comp&&` : `Comp&&` => Unit
    implicit val `Comp^`  : `Comp^` => Unit
    implicit val `Comp==` : `Comp==` => Unit
    implicit val `Comp!=` : `Comp!=` => Unit
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}