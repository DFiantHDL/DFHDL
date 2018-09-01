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
    val i1 = DFUInt(8) <> IN init (1, 1, Bubble, 1)
    val i2 = DFUInt(8) <> IN init (2, Bubble)
    val o1 = DFUInt(8) <> OUT
    val o2 = DFUInt(8) <> OUT
    val b = DFBool() <> IN init (false, true, true, true)
    val myIf = ifdf (b) {
      val myIf2 = ifdf (b) {
        o1 := i1
      }.elseifdf(b) {
        o1 := i1
      }
    }.elsedf {
      o1 := i1
    }
    val ret = DFUInt(8).ifdf (b) {
      DFUInt(8).ifdf (i1 < 8) {
        i1
      }.elsedf {
        i1
      }
    }.elsedf {
      i2
    }
    o2 <> ret
  }

  class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
    final val I = DFUInt(width) <> IN
    final val O = DFUInt(width) <> OUT
    setInitFunc(O)(DFUInt.Token.+(getInit(I), getInit(I)))
  }

  trait Comp extends DFComponent[Comp] {
    val i = DFUInt(8) <> IN
    val o = DFUInt(8) <> OUT
    final protected val foldedDiscoveryDependencyList = (o -> (i :: Nil)) :: Nil
  }
  object Comp {
    implicit val ev : Comp => Unit = ifc => {
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
    implicit val config = DFAnyConfiguration.detailed
    val top_ioDesignIf = new IODesignIf {}
    val compare =
      """
        |trait Relational extends DFDesign {
        |  val inLeft = DFUInt(8) <> IN                               //init = (1, 1, Φ, 1)
        |  val inRight = DFUInt(4) <> IN                              //init = (8)
        |  val outResult = DFBool() <> OUT                            //init = (true, true, Φ, true)
        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTInfixRelationalOp(<)(8, 4) {}
        |  rtInst.A <> inLeft
        |  rtInst.B <> inRight
        |  outResult <> rtInst.S
        |}
        |
        |trait IODesignIf extends DFDesign {
        |  val i1 = DFUInt(8) <> IN init(1, 1, Φ, 1)                  //init = (1, 1, Φ, 1)
        |  val i2 = DFUInt(8) <> IN init(2, Φ)                        //init = (2, Φ)
        |  val o1 = DFUInt(8) <> OUT                                  //init = ()
        |  val o2 = DFUInt(8) <> OUT                                  //init = (2, 1, Φ, 1)
        |  val b = DFBool() <> IN init(false, true, true, true)       //init = (false, true, true, true)
        |  val myIf = ifdf(b) {
        |    val myIf2 = ifdf(b) {
        |      o1 := i1
        |    }.elseifdf(b) {
        |      o1 := i1
        |    }
        |  }.elsedf {
        |    o1 := i1
        |  }
        |  val ret = DFUInt(8) init(2, 1, Φ, 1)                       //init = (2, 1, Φ, 1)
        |  val retǂif = ifdf(b) {
        |    val ǂanon = DFUInt(8) init(1, 1, Φ, 1)                     //init = (1, 1, Φ, 1)
        |    val ǂanonComp = new Relational {}
        |    ǂanonComp.inLeft <> i1
        |    ǂanonComp.inRight <> 8
        |    val ǂanonǂif = ifdf(ǂanonComp.outResult) {
        |      ǂanon := i1
        |    }.elsedf {
        |      ǂanon := i1
        |    }
        |    ret := ǂanon
        |  }.elsedf {
        |    ret := i2
        |  }
        |  o2 <> ret
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
        |  val ǂanon = i.prev                                         //init = (2, 3, 4, Φ)
        |  val ǂanonǂ1 = ǂanon.prev                                   //init = (3, 4, Φ)
        |  val ǂanonǂ2 = ǂanonǂ1.prev                                 //init = (4, Φ)
        |  val ǂanonǂ3 = ǂanonǂ2.prev                                 //init = (Φ)
        |  o <> ǂanonǂ3
        |  val iǂ1 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)              //init = (1, 2, 3, 4, Φ)
        |  val oǂ1 = DFUInt(8) <> OUT                                 //init = (Φ)
        |  val ǂanonǂ4 = iǂ1.prev                                     //init = (2, 3, 4, Φ)
        |  val ǂanonǂ5 = ǂanonǂ4.prev                                 //init = (3, 4, Φ)
        |  val ǂanonǂ6 = ǂanonǂ5.prev                                 //init = (4, Φ)
        |  val ǂanonǂ7 = ǂanonǂ6.prev                                 //init = (Φ)
        |  oǂ1 <> ǂanonǂ7
        |  val iǂ2 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)              //init = (1, 2, 3, 4, Φ)
        |  val oǂ2 = DFUInt(8) <> OUT                                 //init = (Φ)
        |  val ǂanonǂ8 = iǂ2.prev                                     //init = (2, 3, 4, Φ)
        |  val ǂanonǂ9 = ǂanonǂ8.prev                                 //init = (3, 4, Φ)
        |  val ǂanonǂ10 = ǂanonǂ9.prev                                //init = (4, Φ)
        |  val ǂanonǂ11 = ǂanonǂ10.prev                               //init = (Φ)
        |  oǂ2 <> ǂanonǂ11
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
        |  val iǂ1 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)
        |  val oǂ1 = DFUInt(8) <> OUT
        |  oǂ1 <> iǂ1.prev.prev.prev.prev
        |  val iǂ2 = DFUInt(8) <> IN init(1, 2, 3, 4, Φ)
        |  val oǂ2 = DFUInt(8) <> OUT
        |  oǂ2 <> iǂ2.prev.prev.prev.prev
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
        |  val plusOneComp = new Arithmetic {}
        |  plusOneComp.inLeft <> i
        |  plusOneComp.inRight <> 1
        |  val plusOneWC = plusOneComp.outResult                      //init = (6)
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
        |  val checkComp = new Relational {}
        |  checkComp.inLeft <> i2
        |  checkComp.inRight <> i1
        |  o <> checkComp.outResult
        |}
        |
        |val top_ioDesignConn4 = new IODesignConn4 {}
      """.stripMargin
    top_ioDesignConn4.codeString =@= compare
  }

}