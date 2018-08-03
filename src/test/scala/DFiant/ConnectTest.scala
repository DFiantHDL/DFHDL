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

  property("IODesignConn2.codeString") = {
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignConn2 = new IODesignConn2 {}

    val compare =
      """
        |val top_ioDesignConn2 = new DFDesign {  //DFiant.ConnectTest$IODesignConn2
        |  val i = DFUInt(8) <> IN init(1)  //init = (1)
        |  val o = DFUInt(8) <> OUT   //init = (2)
        |  val io = new DFDesign {  //DFiant.ConnectTest$Comp
        |    val i = DFUInt(8) <> IN   //init = (1)
        |    val o = DFUInt(8) <> OUT   //init = (2)
        |    val rt = new DFiant.ConnectTest$RTx2 {}
        |    rt.I <> i
        |    o <> rt.O
        |  }
        |  io.i <> i
        |  o <> io.o
        |}
      """.stripMargin

    println(top_ioDesignConn2.codeString)
    top_ioDesignConn2.codeString =@= compare
  }

}