import DFiant._

import org.scalacheck._
import shapeless.test.illTyped
import TestUtils._
import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._

class ConnectTest extends Properties("ConnectTest") {
  trait IODesignConn1 extends DFDesign {
    val i = DFUInt(8) <> IN init(1,2)
    val o = DFUInt(8) <> OUT
    o <> i
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
    setInitFunc(O)(DFUInt.Token.+(getInit(I), getInit(I)))
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
    val o_wc = DFUInt(9) <> OUT
    val o_c = DFBool() <> OUT
    val plusOne = i + 1
    o <> plusOne
    o_wc <> plusOne.wc
    o_c <> plusOne.c
  }

  trait IODesignConn4 extends DFDesign {
    val i1 = DFUInt(8) <> IN init 8
    val i2 = DFUInt(8) <> IN init 1
    val o = DFBool() <> OUT
    val check = i2 < i1
    o <> check
  }

  trait IODesignConn5 extends DFDesign {
    val myloop = for (i <- 0 to 2) {
      val i = DFUInt(8) <> IN init(1, 2, 3, 4, Bubble)
      val o = DFUInt(8) <> OUT
      o <> i.prev.prev.prev.prev
    }
  }


  property("DFDesign.codeString") = {
    val topIO = new DFDesign {
      val i = DFUInt(8) <> IN init(1, 2)
      val o = DFUInt(8) <> OUT
      o <> i
    }
    val compare =
      """
        |trait DFiant.DFDesign extends DFDesign {
        |  val i = DFUInt(8) <> IN init(1, 2)
        |  val o = DFUInt(8) <> OUT
        |  o <> i
        |}
        |
        |val topIO = new DFiant.DFDesign {}
      """.stripMargin
    topIO.codeString =@= compare
  }

  property("ContainerConn3.codeString") = {
    val top_containerConn3 = new ContainerConn3 {}
    val compare =
      """
        |trait IODesignConn1 extends DFDesign {
        |  val i = DFUInt(8) <> IN init(1, 2)
        |  val o = DFUInt(8) <> OUT
        |  o <> i
        |}
        |
        |trait ContainerConn3 extends DFDesign {
        |  val i = DFUInt(8) <> IN
        |  val o = DFUInt(8) <> OUT
        |  val io1 = new IODesignConn1 {}
        |  val io2 = new IODesignConn1 {}
        |  io1.i <> i
        |  io2.i <> io1.o
        |  o <> io2.o
        |}
        |
        |val top_containerConn3 = new ContainerConn3 {}
      """.stripMargin
    top_containerConn3.codeString =@= compare
  }

  property("IODesignIf.codeString") = {
    val top_ioDesignIf = new IODesignIf {}
    val compare =
      """
        |trait IODesignIf extends DFDesign {
        |  val i = DFUInt(8) <> IN
        |  val o = DFUInt(8) <> OUT
        |  val b = DFBool() <> IN
        |  val myIf = ifdf(b) {
        |    val myIf2 = ifdf(b) {
        |      o := i
        |    }.elseifdf(b) {
        |      o := i
        |    }
        |  }.elsedf {
        |    o := i
        |  }
        |  val bb = DFBool()
        |}
        |
        |val top_ioDesignIf = new IODesignIf {}
      """.stripMargin
    top_ioDesignIf.codeString =@= compare
  }

  property("IODesignConn2.codeString") = {
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignConn2 = new IODesignConn2 {}
    val compare =
      """
        |trait $anon$1 extends DFDesign {
        |  val i = DFUInt(8) <> IN                                    //init = (1)
        |  val o = DFUInt(8) <> OUT                                   //init = (2)
        |  val rt = new ConnectTest$RTx2(8) {}
        |  rt.I <> i
        |  o <> rt.O
        |}
        |
        |trait IODesignConn2 extends DFDesign {
        |  val i = DFUInt(8) <> IN init(1)                            //init = (1)
        |  val o = DFUInt(8) <> OUT                                   //init = (2)
        |  val io = new $anon$1 {}
        |  io.i <> i
        |  o <> io.o
        |}
        |
        |val top_ioDesignConn2 = new IODesignConn2 {}
      """.stripMargin
    top_ioDesignConn2.codeString =@= compare
  }

  property("IODesignConn5.codeString detailed") = {
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignConn5 = new IODesignConn5 {}
    val compare =
      """
        |trait IODesignConn5 extends DFDesign {
        |  val i = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)                //init = (1, 2, 3, 4, Φ)
        |  val o = DFUInt(8) <> OUT                                   //init = (Φ)
        |  val $anon = i.prev                                         //init = (2, 3, 4, Φ)
        |  val $anon$1 = $anon.prev                                   //init = (3, 4, Φ)
        |  val $anon$2 = $anon$1.prev                                 //init = (4, Φ)
        |  val $anon$3 = $anon$2.prev                                 //init = (Φ)
        |  o <> $anon$3
        |  val i$1 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)              //init = (1, 2, 3, 4, Φ)
        |  val o$1 = DFUInt(8) <> OUT                                 //init = (Φ)
        |  val $anon$4 = i$1.prev                                     //init = (2, 3, 4, Φ)
        |  val $anon$5 = $anon$4.prev                                 //init = (3, 4, Φ)
        |  val $anon$6 = $anon$5.prev                                 //init = (4, Φ)
        |  val $anon$7 = $anon$6.prev                                 //init = (Φ)
        |  o$1 <> $anon$7
        |  val i$2 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)              //init = (1, 2, 3, 4, Φ)
        |  val o$2 = DFUInt(8) <> OUT                                 //init = (Φ)
        |  val $anon$8 = i$2.prev                                     //init = (2, 3, 4, Φ)
        |  val $anon$9 = $anon$8.prev                                 //init = (3, 4, Φ)
        |  val $anon$10 = $anon$9.prev                                //init = (4, Φ)
        |  val $anon$11 = $anon$10.prev                               //init = (Φ)
        |  o$2 <> $anon$11
        |}
        |
        |val top_ioDesignConn5 = new IODesignConn5 {}
      """.stripMargin
    top_ioDesignConn5.codeString =@= compare
  }

  property("IODesignConn5.codeString default") = {
    val top_ioDesignConn5 = new IODesignConn5 {}
    val compare =
      """
        |trait IODesignConn5 extends DFDesign {
        |  val i = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)
        |  val o = DFUInt(8) <> OUT
        |  o <> i.prev.prev.prev.prev
        |  val i$1 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)
        |  val o$1 = DFUInt(8) <> OUT
        |  o$1 <> i$1.prev.prev.prev.prev
        |  val i$2 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)
        |  val o$2 = DFUInt(8) <> OUT
        |  o$2 <> i$2.prev.prev.prev.prev
        |}
        |
        |val top_ioDesignConn5 = new IODesignConn5 {}
      """.stripMargin
    top_ioDesignConn5.codeString =@= compare
  }

  property("IODesignConn3.codeString detailed") = {
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignConn3 = new IODesignConn3 {}
    val compare =
      """
        |trait Arithmetic extends DFDesign {
        |  val inLeft = DFUInt(8) <> IN                               //init = (5)
        |  val inRight = DFUInt(1) <> IN                              //init = (1)
        |  val outResult = DFUInt(9) <> OUT                           //init = (6)
        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTAdd(8, 1, 9) {}
        |  rtInst.A <> inLeft
        |  rtInst.B <> inRight
        |  outResult <> rtInst.S
        |}
        |
        |trait IODesignConn3 extends DFDesign {
        |  val i = DFUInt(8) <> IN init(5)                            //init = (5)
        |  val o = DFUInt(8) <> OUT                                   //init = (6)
        |  val o_wc = DFUInt(9) <> OUT                                //init = (6)
        |  val o_c = DFBool() <> OUT                                  //init = (false)
        |  val opInst = new Arithmetic {}
        |  opInst.inLeft <> i
        |  opInst.inRight <> 1
        |  val plusOneWC = opInst.outResult                           //init = (6)
        |  val plusOne = plusOneWC.bits(7, 0).uint                    //init = (6)
        |  o <> plusOne
        |  o_wc <> plusOneWC
        |  val plusOneC = plusOneWC.bit(8)                            //init = (false)
        |  o_c <> plusOneC
        |}
        |
        |val top_ioDesignConn3 = new IODesignConn3 {}
      """.stripMargin
    top_ioDesignConn3.codeString =@= compare
  }

  property("IODesignConn4.codeString detailed") = {
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignConn4 = new IODesignConn4 {}
    val compare =
      """
        |trait Relational extends DFDesign {
        |  val inLeft = DFUInt(8) <> IN                               //init = (1)
        |  val inRight = DFUInt(8) <> IN                              //init = (8)
        |  val outResult = DFBool() <> OUT                            //init = (true)
        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTInfixRelationalOp(<)(8, 8) {}
        |  rtInst.A <> inLeft
        |  rtInst.B <> inRight
        |  outResult <> rtInst.S
        |}
        |
        |trait IODesignConn4 extends DFDesign {
        |  val i1 = DFUInt(8) <> IN init(8)                           //init = (8)
        |  val i2 = DFUInt(8) <> IN init(1)                           //init = (1)
        |  val o = DFBool() <> OUT                                    //init = (true)
        |  val opInst = new Relational {}
        |  opInst.inLeft <> i2
        |  opInst.inRight <> i1
        |  o <> opInst.outResult
        |}
        |
        |val top_ioDesignConn4 = new IODesignConn4 {}
      """.stripMargin
    top_ioDesignConn4.codeString =@= compare
  }

}