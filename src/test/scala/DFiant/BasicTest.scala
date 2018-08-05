package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._

trait IODesignConn1 extends DFDesign {
  val i = DFUInt(8) <> IN init(1,2)
  val o = DFUInt(8) <> OUT
  o <> i
}

trait IODesignIf extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val b = DFBool() <> IN
  val myIf = ifdf (b) {
    val myIf2 = ifdf (b) {
      o := i
    }.elseifdf(b) {
      o := i
    }
  }.elsedf {
    o := i
  }
  val bb = DFBool()
  bb.keep
}

class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
  private lazy val OInit = () => DFUInt.Token.+(getInit(I), getInit(I))
  setInitFunc(O)(OInit)
}

trait Comp extends DFComponent[Comp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
}
object Comp {
  implicit val ev : DFComponent.Implementation[Comp] = ifc => {
    import ifc._
    val rt = new RTx2(8)
    rt.I <> i
    rt.O <> o
  }
}

trait IODesignConn2 extends DFDesign{
  val i = DFUInt(8) <> IN init 1
  val o = DFUInt(8) <> OUT

  val io = new Comp {}
  i <> io.i
  o <> io.o
}


trait IODesignConn3 extends DFDesign {
  val i = DFUInt(8) <> IN init 5
  val o = DFUInt(8) <> OUT
  val plusOne = i + 1
  o <> plusOne
}

class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val A = DFUInt(aWidth) <> IN
  final val B = DFUInt(bWidth) <> IN
  final val S = DFUInt(sWidth) <> OUT
  private lazy val SInit = () => DFUInt.Token.+(getInit(A), getInit(B))
  setInitFunc(S)(SInit)
}


trait IODesignConn4 extends DFDesign {
  val i = DFUInt(8) <> IN init(1, 2, 3, 4, Bubble)
  val o = DFUInt(8) <> OUT
//  val temp = i.prev()
  o <> i.prev.prev.prev.prev
}

trait IODesignConn5 extends DFDesign {
  val myloop = for (i <- 0 to 2) {
    val i = DFUInt(8) <> IN init(1, 2, 3, 4, Bubble)
    val o = DFUInt(8) <> OUT
    o <> i.prev.prev.prev.prev
  }
}

trait ContainerConn1 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesignConn1 {}
  i    <> io.i //Connecting between owner input and child input
  io.o.prev <> o    //Connecting between child output and owner output
}

trait ContainerConnLoop extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesignConn1 {}
  io.i <> io.o
  o <> io.o
}

trait ContainerConn3 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io1 = new IODesignConn1 {}
  val io2 = new IODesignConn1 {}
  i     <> io1.i //Connecting between owner input and child input
  io1.o <> io2.i //Connecting between siblings (output <> input)
  io2.o <> o     //Connecting between child output and owner output
}

trait ContainerConn4 extends DFDesign {
  val i = DFUInt(7) <> IN
  val o = DFUInt(8) <> OUT
  val ob = DFBool() <> OUT
  val io = new IODesignConn1 {}
  i <> io.i
  io.o <> o
  true <> ob
}

//trait IODesign extends DFDesign {
//  val i = DFUInt(8) <> IN
//  val o = DFUInt(8) <> OUT
//  o := i
//}
//
//trait IODesign1 extends DFDesign {
//  val i = DFUInt(8) <> IN
//  val o = DFUInt(8) <> OUT
//  val tmp = DFUInt(8)
//  tmp := i
//  o := tmp
//}
//
//trait IODesign2 extends DFDesign {
//  val i1 = DFUInt(8) <> IN
//  val o1 = DFUInt(8) <> OUT
//  val i2 = DFUInt(8) <> IN
//  val o2 = DFUInt(8) <> OUT
//  o1 := i1
//  o2 := i2
//}
//
//



object BasicTest extends App {
  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
//  implicit val a = DFAnyConfiguration.detailed
//  val top_ioDesignConn1 = new IODesignConn1 {}
//  val top_ioDesignConn2 = new IODesignConn2 {}
//  val top_ioDesignConn3 = new IODesignConn3 {}
//  val top_ioDesignConn4 = new IODesignConn4 {}
//  val top_ioDesignConn5 = new IODesignConn5 {}
//  val top_containerConn1 = new ContainerConn1 {}
//  val top_containerConn3 = new ContainerConn3 {}
//  val top_containerConn4 = new ContainerConn4 {}
  val top_ioDesignIf = new IODesignIf {}
  println(top_ioDesignIf.codeString)

}

