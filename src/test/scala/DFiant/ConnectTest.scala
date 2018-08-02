package DFiant

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

  property("DFDesign.codeString") = {
    val topIO = new DFDesign {
      val i = DFUInt(8) <> IN init(1, 2)
      val o = DFUInt(8) <> OUT
      o <> i
    }
    val compare =
      """
        |val topIO = new DFDesign {
        |  val i = DFUInt(8) <> IN init(1, 2)
        |  val o = DFUInt(8) <> OUT
        |  o <> i
        |}
      """.stripMargin

    topIO.codeString =@= compare
  }

  property("ContainerConn3.codeString") = {
    val top_containerConn3 = new ContainerConn3 {}

    val compare =
      """
        |val top_containerConn3 = new DFDesign {
        |  val i = DFUInt(8) <> IN
        |  val o = DFUInt(8) <> OUT
        |  val io1 = new DFDesign {
        |    val i = DFUInt(8) <> IN init(1, 2)
        |    val o = DFUInt(8) <> OUT
        |    o <> i
        |  }
        |  val io2 = new DFDesign {
        |    val i = DFUInt(8) <> IN init(1, 2)
        |    val o = DFUInt(8) <> OUT
        |    o <> i
        |  }
        |  io1.i <> i
        |  io2.i <> io1.o
        |  o <> io2.o
        |}
      """.stripMargin

    top_containerConn3.codeString =@= compare
  }

  property("IODesignIf.codeString") = {
    val top_ioDesignIf = new IODesignIf {}

    val compare =
      """
        |val top_ioDesignIf = new DFDesign {
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
      """.stripMargin

    top_ioDesignIf.codeString =@= compare
  }

}