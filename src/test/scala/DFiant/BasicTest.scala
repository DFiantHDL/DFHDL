import DFiant._
import DFiant.basiclib.DFBasicLib
import DFiant.basiclib.DFUIntOps.`Comp+`

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
  val retIf = DFUInt(8).ifdf (b) {
    i
  }
  val bb = DFBool()
  bb.keep
}

class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val I = DFUInt(width) <> IN
  final val O = DFUInt(width) <> OUT
  setInitFunc(O)(DFUInt.Token.+(getInit(I), getInit(I)))
}

trait Comp extends DFComponent[Comp] {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  setInitFunc(o)(DFUInt.Token.+(getInit(i), getInit(i)))
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
  val plusTwo = plusOne + 1
  o <> plusTwo
}

class RTAdd(aWidth : Int, bWidth : Int, sWidth : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val A = DFUInt(aWidth) <> IN
  final val B = DFUInt(bWidth) <> IN
  final val S = DFUInt(sWidth) <> OUT
  setInitFunc(S)(DFUInt.Token.+(getInit(A), getInit(B)))
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

trait IODesignConn6 extends DFDesign {
  val in = DFBits(7) <> IN init b"10000000"
  val out = DFBits(9) <> OUT

  out <> b"0" ## in ## b"1"
}

trait IODesignConn7 extends DFDesign {
  val in = DFBits(8) <> IN init b"00000010"
  val out = DFUInt(8) <> OUT

  val s = in.as(DFUInt(8))
  out <> s
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

object Foo extends Enum.Auto {
  val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
}

trait IODesignConn8 extends DFDesign {
  val i = DFEnum(Foo) <> IN// init Foo.Baz3
  val o = DFEnum(Foo) <> OUT
  o := i
}
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
  implicit val a = DFAnyConfiguration.detailed
//  val top_ioDesignConn1 = new IODesignConn1 {}
//  val top_ioDesignConn2 = new IODesignConn2 {}
//  val top_ioDesignConn3 = new IODesignConn3 {}
//  val top_ioDesignConn4 = new IODesignConn4 {}
//  val top_ioDesignConn5 = new IODesignConn5 {}
//  val top_ioDesignConn6 = new IODesignConn6 {}
//    val top_ioDesignConn7 = new IODesignConn7 {}
//    val top_ioDesignConn8 = new IODesignConn8 {}
//  val top_containerConn1 = new ContainerConn1 {}
//  val top_containerConn3 = new ContainerConn3 {}
//  val top_containerConn4 = new ContainerConn4 {}
  val top_ioDesignIf = new IODesignIf {}
//  println(top_ioDesignConn2.codeString)
  println(top_ioDesignIf.codeString)


//  trait MyDesign extends DFDesign{
//    val in = DFSInt(4) <> IN init -1
//    val out = DFSInt(8) <> OUT
//
//    out <> (b"1111", in).bits.sint
//  }
//
//  val myDesign = new MyDesign {}
//  println(myDesign.codeString)
}
