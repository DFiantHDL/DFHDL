package DFiant

object BasicTest extends App {

  trait MemeDesign extends DFDesign {
    type W = 8
    val meme = new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
      val aa = DFUInt[W] <> IN(a)
      def implementation(): Unit = {}
    }
    new DFDesign {
      val a : DFUInt[W] <> IN = OPEN
      def implementation(): Unit = {}
    }
  }

  trait IODesign extends DFDesign {
    type W = 8
    val i : DFUInt[W] <> IN
    val o : DFUInt[W] <> OUT
    val tmp = DFUInt[W]
    def implementation() : Unit = {
      tmp := i
      o := tmp
    }
  }

  trait IOComp extends DFComponent[IOComp] {
    type W = 8
    val i : DFUInt[W] <> IN
    val o : DFUInt[W] <> OUT
  }

  object IOComp {
    implicit def ev : DFComponent.Implementation[IOComp] = ifc => {
      import ifc._
      val tmp = DFUInt[W]
      val rt = new RTIOComp {
        val i = ifc.i
        val o = tmp
      }

      o := tmp
    }

  }

  trait RTIOComp extends RTComponent {
    type W = 8
    val i : DFUInt[W] <> IN
    val o : DFUInt[W] <> OUT
  }

  trait MyDesign extends DFDesign {
    type W = 8
    val a_in : DFUInt[W] <> IN = TOP
    val b_in : DFUInt[W] <> IN = TOP
    val c_in : DFUInt[W] <> IN = TOP
    val d_in : DFUInt[W] <> IN = TOP
    val e_in : DFUInt[W] <> IN = TOP
    val a_out : DFUInt[W] <> OUT = TOP
    val b_out : DFUInt[W] <> OUT = TOP
    val c_out : DFUInt[W] <> OUT = TOP
    val d_out : DFUInt[W] <> OUT = TOP
    val e_out : DFUInt[W] <> OUT = TOP

    val a_io = new DFDesign {
      a_io =>
      val i : DFUInt[W] <> IN = a_in
      val o : DFUInt[W] <> OUT = a_out
      def implementation(): Unit = {
        val internal = new DFDesign {
          val i : DFUInt[W] <> IN = a_io.i
          val o : DFUInt[W] <> OUT = a_io.o
          val tmp = DFUInt[W]
          def implementation(): Unit = {
            tmp := i.prev()
            o := tmp
          }
        }

      }
    }

    val b_io = new IODesign {
      val i = b_in
      val o = b_out
    }

    val c_io = new IOComp {
      val i = c_in
      val o = c_out
    }

    val d_io = new RTIOComp {
      val i = d_in
      val o = d_out
    }


    def implementation(): Unit = {
      e_out := e_in + e_in
    }
  }

  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  val myDesign = new MyDesign {}
  println(myDesign.portsOut)
//  myDesign.meme.keep
//  myDesign.wakeLazyAlmanac
//  myDesign.protAlmanac.printEntrees()
//  println(myDesign.meme.name)
//  myDesign.keep
//  println(myDesign.components)

  myDesign.printInfo()
  println(myDesign.a_io.portNodes)

//  myDesign.compileToVHDL("myDesignTest")
}
