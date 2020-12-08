///*
// *     This file is part of DFiant.
// *
// *     DFiant is free software: you can redistribute it and/or modify
// *     it under the terms of the GNU Lesser General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFiant is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     GNU Lesser General Public License for more details.
// *
// *     You should have received a copy of the GNU Lesser General Public License
// *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//import DFiant._
//import DFiant.internals.{devAccess}
//import org.scalacheck._
//import shapeless.test.illTyped
//import TestUtils._
//import UnofficialXilinx.FPGAs.`XC7VX485T-2FFG1761C`._
//
//class ConnectTest extends Properties("ConnectTest") {
//  trait IODesignConn1 extends DFDesign {
//    val i = DFUInt(8) <> IN init(1,2)
//    val o = DFUInt(8) <> OUT
//    o <> i
//  }
//
//  trait ContainerConn3 extends DFDesign {
//    val i = DFUInt(8) <> IN
//    val o = DFUInt(8) <> OUT
//    val io1 = new IODesignConn1 {}
//    val io2 = new IODesignConn1 {}
//    i     <> io1.i //Connecting between owner input and child input
//    io1.o <> io2.i //Connecting between siblings (output <> input)
//    io2.o <> o     //Connecting between child output and owner output
//  }
//
//  trait IODesignIf extends DFDesign {
//    val i1 = DFUInt(8) <> IN init (1, 1, Bubble, 1)
//    val i2 = DFUInt(8) <> IN init (2, Bubble)
//    val o1 = DFUInt(8) <> OUT
//    val o2 = DFUInt(8) <> OUT
//    val b = DFBool() <> IN init (false, true, true, true)
//    val myIf = ifdf (b) {
//      val myIf2 = ifdf (b) {
//        o1 := i1
//      }.elseifdf(b) {
//        o1 := i1
//      }
//    }.elsedf {
//      o1 := i1
//    }
//    val ret = DFUInt(8).ifdf (b) {
//      val ret2 = DFUInt(8).ifdf (i1 < 8) {
//        i1
//      }.elseifdf(b) {
//        i2
//      }.elsedf {
//        i1
//      }
//      ret2
//    }.elsedf {
//      i2
//    }
//    o2 <> ret
//  }
//
//  class RTx2(width : Int)(implicit ctx : RTComponent.Context) extends RTComponent {
//    final val I = DFUInt(width) <> IN
//    final val O = DFUInt(width) <> OUT
//    final override protected val blackBoxFunctions = Map(O -> BlackBoxFunction(O)(I, I)((l, r) => l + r))
//  }
//
//  trait Comp extends DFComponent[Comp] {
//    val i = DFUInt(8) <> IN
//    val o = DFUInt(8) <> OUT
//    final override protected val blackBoxFunctions = Map(o -> BlackBoxFunction(o)(i, i)((l, r) => l + r))
//  }
//  object Comp {
//    implicit val ev : Comp => Unit = ifc => {
//      import ifc._
//      val rt = new RTx2(8)
//      rt.I <> i
//      rt.O <> o
//    }
//  }
//
//  trait IODesignConn2 extends DFDesign{
//    val i = DFUInt(8) <> IN init 1
//    val o = DFUInt(8) <> OUT
//
//    val io = new Comp {}
//    i <> io.i
//    o <> io.o
//  }
//
//  trait IODesignConn3 extends DFDesign {
//    val i = DFUInt(8) <> IN init 5
//    val o = DFUInt(8) <> OUT
//    val o_wc = DFUInt(9) <> OUT
//    val o_c = DFBool() <> OUT
//    val plusOne = i + 1
//    o <> plusOne
//    o_wc <> plusOne.wc
//    o_c <> plusOne.c
//  }
//
//  trait IODesignConn4 extends DFDesign {
//    val i1 = DFUInt(8) <> IN init 8
//    val i2 = DFUInt(8) <> IN init 1
//    val o = DFBool() <> OUT
//    val check = i2 < i1
//    o <> check
//  }
//
//  trait IODesignConn5 extends DFDesign {
//    val myloop = for (i <- 0 to 2) {
//      val i = DFUInt(8) <> IN init(1, 2, 3, 4, Bubble)
//      val o = DFUInt(8) <> OUT
//      o <> i.prev.prev.prev.prev
//    }
//  }
//
//  trait IODesignMatch extends DFDesign {
//    val i1 = DFUInt(8) <> IN init (1, 1, Bubble, 1)
//    val i2 = DFUInt(8) <> IN init (2, 8, 7, 11, 21)
//    val o1 = DFUInt(8) <> OUT
//    val myMatch = matchdf (i2, MatchConfig.AllowOverlappingCases)
//      .casedf(1 to 5, 10 to 20) {o1 := i1}
//      .casedf(7){o1 := i2}
//      .casedf(11){o1 := i2}
//      .casedf_{o1 := i2}
//
//    val o2 = DFUInt(8) <> OUT
//    val ret = DFUInt(8).matchdf(i2)
//      .casedf(1 to 5, 10 to 20) {i1}
//      .casedf(7){75}
//      .casedf_{88}
//    o2 <> ret
//
//    val i3 = DFEnum(Foo) <> IN init (Foo.Baz0, Foo.Baz3)
//    val o3 = DFUInt(8) <> OUT
//    val myEnumMatch = matchdf (i3)
//      .casedf(Foo.Baz0) {o3 := 1}
//      .casedf(Foo.Baz1) {o3 := 0}
//  }
//
//
//
//  property("DFDesign.codeString") = {
//    import DFDesign.allowTop._
//    val topIO = new DFDesign { //TODO: consider fixing name of anonymous DFDesign
//      val i = DFUInt(8) <> IN init(1, 2)
//      val o = DFUInt(8) <> OUT
//      o <> i
//    }
//    val compare =
//      """
//        |trait DFDesign extends DFDesign {
//        |  val i = DFUInt(8) <> IN init(1, 2)
//        |  val o = DFUInt(8) <> OUT
//        |  o <> i
//        |}
//        |
//        |val topIO = new DFDesign {}
//      """.stripMargin
//    topIO.codeString =@= compare
//  }
//
//  property("ContainerConn3.codeString") = {
//    import DFDesign.allowTop._
//    val top_containerConn3 = new ContainerConn3 {}
//    val compare =
//      """
//        |trait IODesignConn1 extends DFDesign {
//        |  val i = DFUInt(8) <> IN init(1, 2)
//        |  val o = DFUInt(8) <> OUT
//        |  o <> i
//        |}
//        |
//        |trait ContainerConn3 extends DFDesign {
//        |  val i = DFUInt(8) <> IN
//        |  val o = DFUInt(8) <> OUT
//        |  val io1 = new IODesignConn1 {}
//        |  val io2 = new IODesignConn1 {}
//        |  io1.i <> i
//        |  io2.i <> io1.o
//        |  o <> io2.o
//        |}
//        |
//        |val top_containerConn3 = new ContainerConn3 {}
//      """.stripMargin
//    top_containerConn3.codeString =@= compare
//  }
//
////  property("IODesignIf.codeString detailed") = {
////    implicit val config = DFAnyConfiguration.detailed
////    val top_ioDesignIf = new IODesignIf {}
////    val compare =
////      """
////        |trait Relational extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (1, 1, ?, 1)
////        |  val inRight = DFUInt(4) <> IN                              //init = (8)
////        |  val outResult = DFBool() <> OUT                            //init = (true, true, ?, true)
////        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTInfixRelationalOp(<)(8, 4) {}
////        |  rtInst.A <> inLeft
////        |  rtInst.B <> inRight
////        |  outResult <> rtInst.S
////        |}
////        |
////        |trait `Func2Comp<` extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (1, 1, ?, 1)
////        |  val inRight = DFUInt(4) <> IN                              //init = (8)
////        |  val outResult = DFBool() <> OUT                            //init = (true, true, ?, true)
////        |  val dFt_anon = new Relational {}
////        |  dFt_anon.inLeft <> inLeft
////        |  dFt_anon.inRight <> inRight
////        |  outResult <> dFt_anon.outResult
////        |}
////        |
////        |trait IODesignIf extends DFDesign {
////        |  val i1 = DFUInt(8) <> IN init(1, 1, ?, 1)                  //init = (1, 1, ?, 1)
////        |  val i2 = DFUInt(8) <> IN init(2, ?)                        //init = (2, ?)
////        |  val o1 = DFUInt(8) <> OUT                                  //init = ()
////        |  val o2 = DFUInt(8) <> OUT                                  //init = (2, 1, ?, 1)
////        |  val b = DFBool() <> IN init(false, true, true, true)       //init = (false, true, true, true)
////        |  val myIf = ifdf(b) {
////        |    val myIf2 = ifdf(b) {
////        |      o1 := i1
////        |    }.elseifdf(b) {
////        |      o1 := i1
////        |    }
////        |  }.elsedf {
////        |    o1 := i1
////        |  }
////        |  val ret = DFUInt(8) init(2, 1, ?, 1)                       //init = (2, 1, ?, 1)
////        |  val ret_d_if = ifdf(b) {
////        |    val dFt_anon = DFUInt(8) init(1, 1, ?, 1)                  //init = (1, 1, ?, 1)
////        |    val dFt_anon_d_1 = new `Func2Comp<` {}
////        |    dFt_anon_d_1.inLeft <> i1
////        |    dFt_anon_d_1.inRight <> 8
////        |    val dFt_anon_d_if = ifdf(dFt_anon_d_1.outResult) {
////        |      dFt_anon := i1
////        |    }.elseifdf(b) {
////        |      dFt_anon := i2
////        |    }.elsedf {
////        |      dFt_anon := i1
////        |    }
////        |    ret := dFt_anon
////        |  }.elsedf {
////        |    ret := i2
////        |  }
////        |  o2 <> ret
////        |}
////        |
////        |val top_ioDesignIf = new IODesignIf {}
////      """.stripMargin
////    top_ioDesignIf.codeString =@= compare
////  }
//
////  property("IODesignIf.codeString default") = {
////    import DFDesign.allowTop._
////    val top_ioDesignIf = new IODesignIf {}
////    val compare =
////      """
////        |trait IODesignIf extends DFDesign {
////        |  val i1 = DFUInt(8) <> IN init(1, 1, ?, 1)
////        |  val i2 = DFUInt(8) <> IN init(2, ?)
////        |  val o1 = DFUInt(8) <> OUT
////        |  val o2 = DFUInt(8) <> OUT
////        |  val b = DFBool() <> IN init(false, true, true, true)
////        |  ifdf(b) {
////        |    ifdf(b) {
////        |      o1 := i1
////        |    }.elseifdf(b) {
////        |      o1 := i1
////        |    }
////        |  }.elsedf {
////        |    o1 := i1
////        |  }
////        |  val ret = DFUInt(8) init(2, 1, ?, 1)
////        |  ifdf(b) {
////        |    val ret2 = DFUInt(8) init(1, 1, ?, 1)
////        |    ifdf(i1 < 8) {
////        |      ret2 := i1
////        |    }.elseifdf(b) {
////        |      ret2 := i2
////        |    }.elsedf {
////        |      ret2 := i1
////        |    }
////        |    ret := ret2
////        |  }.elsedf {
////        |    ret := i2
////        |  }
////        |  o2 <> ret
////        |}
////        |
////        |val top_ioDesignIf = new IODesignIf {}
////      """.stripMargin
////    top_ioDesignIf.codeString =@= compare
////  }
//
////  property("IODesignConn2.codeString") = {
////    implicit val config = DFAnyConfiguration.detailed
////    import DFDesign.allowTop._
////    val top_ioDesignConn2 = new IODesignConn2 {}
////    top_ioDesignConn2.io.unfold
////    val compare =
////      """
////        |trait Comp extends DFDesign {
////        |  val i = DFUInt(8) <> IN                                    //init = (1)
////        |  val o = DFUInt(8) <> OUT                                   //init = (2)
////        |  val rt = new RTx2 {}
////        |  rt.I <> i
////        |  o <> rt.O
////        |}
////        |
////        |trait IODesignConn2 extends DFDesign {
////        |  val i = DFUInt(8) <> IN init(1)                            //init = (1)
////        |  val o = DFUInt(8) <> OUT                                   //init = (2)
////        |  val io = new Comp {}
////        |  io.i <> i
////        |  o <> io.o
////        |}
////        |
////        |val top_ioDesignConn2 = new IODesignConn2 {}
////      """.stripMargin
////    top_ioDesignConn2.codeString =@= compare
////  }
//
//  property("IODesignConn5.codeString detailed") = {
//    implicit val config = DFAnyConfiguration.detailed
//    import DFDesign.allowTop._
//    val top_ioDesignConn5 = new IODesignConn5 {}
//    val compare =
//      """
//        |trait IODesignConn5 extends DFDesign {
//        |  val i_0 = DFUInt(8) <> IN init(1, 2, 3, 4, ?)              //init = (1, 2, 3, 4, ?)
//        |  val o_0 = DFUInt(8) <> OUT                                 //init = (?)
//        |  val dFt_myloop = i_0.prev                                  //init = (2, 3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (4, ?)
//        |  val myloop_0 = dFt_myloop.prev                             //init = (?)
//        |  o_0 <> myloop_0
//        |  val i_1 = DFUInt(8) <> IN init(1, 2, 3, 4, ?)              //init = (1, 2, 3, 4, ?)
//        |  val o_1 = DFUInt(8) <> OUT                                 //init = (?)
//        |  val dFt_myloop = i_1.prev                                  //init = (2, 3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (4, ?)
//        |  val myloop_1 = dFt_myloop.prev                             //init = (?)
//        |  o_1 <> myloop_1
//        |  val i_2 = DFUInt(8) <> IN init(1, 2, 3, 4, ?)              //init = (1, 2, 3, 4, ?)
//        |  val o_2 = DFUInt(8) <> OUT                                 //init = (?)
//        |  val dFt_myloop = i_2.prev                                  //init = (2, 3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (3, 4, ?)
//        |  val dFt_myloop = dFt_myloop.prev                           //init = (4, ?)
//        |  val myloop_2 = dFt_myloop.prev                             //init = (?)
//        |  o_2 <> myloop_2
//        |}
//        |
//        |val top_ioDesignConn5 = new IODesignConn5 {}
//      """.stripMargin
//    top_ioDesignConn5.codeString =@= compare
//  }
//
////  property("IODesignConn5.codeString default") = {
////    import DFDesign.allowTop._
////    val top_ioDesignConn5 = new IODesignConn5 {}
////    val compare =
////      """
////        |trait IODesignConn5 extends DFDesign {
////        |  val i = DFUInt(8) <> IN init(1, 2, 3, 4, ?)
////        |  val o = DFUInt(8) <> OUT
////        |  o <> i.prev.prev.prev.prev
////        |  val i_d_1 = DFUInt(8) <> IN init(1, 2, 3, 4, ?)
////        |  val o_d_1 = DFUInt(8) <> OUT
////        |  o_d_1 <> i_d_1.prev.prev.prev.prev
////        |  val i_d_2 = DFUInt(8) <> IN init(1, 2, 3, 4, ?)
////        |  val o_d_2 = DFUInt(8) <> OUT
////        |  o_d_2 <> i_d_2.prev.prev.prev.prev
////        |}
////        |
////        |val top_ioDesignConn5 = new IODesignConn5 {}
////      """.stripMargin
////    top_ioDesignConn5.codeString =@= compare
////  }
//
////  property("IODesignConn3.codeString detailed") = {
////    implicit val config = DFAnyConfiguration.detailed
////    val top_ioDesignConn3 = new IODesignConn3 {}
////    val compare =
////      """
////        |trait Arithmetic extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (5)
////        |  val inRight = DFUInt(1) <> IN                              //init = (1)
////        |  val outResult = DFUInt(9) <> OUT                           //init = (6)
////        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTAdd(8, 1, 9) {}
////        |  rtInst.A <> inLeft
////        |  rtInst.B <> inRight
////        |  outResult <> rtInst.S
////        |}
////        |
////        |trait `Func2Comp+` extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (5)
////        |  val inRight = DFUInt(1) <> IN                              //init = (1)
////        |  val outResult = DFUInt(9) <> OUT                           //init = (6)
////        |  val dFt_anon = new Arithmetic {}
////        |  dFt_anon.inLeft <> inLeft
////        |  dFt_anon.inRight <> inRight
////        |  outResult <> dFt_anon.outResult
////        |}
////        |
////        |trait IODesignConn3 extends DFDesign {
////        |  val i = DFUInt(8) <> IN init(5)                            //init = (5)
////        |  val o = DFUInt(8) <> OUT                                   //init = (6)
////        |  val o_wc = DFUInt(9) <> OUT                                //init = (6)
////        |  val o_c = DFBool() <> OUT                                  //init = (false)
////        |  val plusOneWC = new `Func2Comp+` {}
////        |  val plusOne = plusOneWC.outResult.bits(7, 0).uint          //init = (6)
////        |  plusOneWC.inLeft <> i
////        |  plusOneWC.inRight <> 1
////        |  o <> plusOne
////        |  o_wc <> plusOneWC.outResult
////        |  val plusOneC = plusOneWC.outResult.bit(8)                  //init = (false)
////        |  o_c <> plusOneC
////        |}
////        |
////        |val top_ioDesignConn3 = new IODesignConn3 {}
////      """.stripMargin
////    top_ioDesignConn3.codeString =@= compare
////  }
//
////  property("IODesignConn4.codeString detailed") = {
////    implicit val config = DFAnyConfiguration.detailed
////    val top_ioDesignConn4 = new IODesignConn4 {}
////    val compare =
////      """
////        |trait Relational extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (1)
////        |  val inRight = DFUInt(8) <> IN                              //init = (8)
////        |  val outResult = DFBool() <> OUT                            //init = (true)
////        |  val rtInst = new Xilinx.Series$basicLib$DFUIntOps$RTInfixRelationalOp(<)(8, 8) {}
////        |  rtInst.A <> inLeft
////        |  rtInst.B <> inRight
////        |  outResult <> rtInst.S
////        |}
////        |
////        |trait `Func2Comp<` extends DFDesign {
////        |  val inLeft = DFUInt(8) <> IN                               //init = (1)
////        |  val inRight = DFUInt(8) <> IN                              //init = (8)
////        |  val outResult = DFBool() <> OUT                            //init = (true)
////        |  val dFt_anon = new Relational {}
////        |  dFt_anon.inLeft <> inLeft
////        |  dFt_anon.inRight <> inRight
////        |  outResult <> dFt_anon.outResult
////        |}
////        |
////        |trait IODesignConn4 extends DFDesign {
////        |  val i1 = DFUInt(8) <> IN init(8)                           //init = (8)
////        |  val i2 = DFUInt(8) <> IN init(1)                           //init = (1)
////        |  val o = DFBool() <> OUT                                    //init = (true)
////        |  val check = new `Func2Comp<` {}
////        |  check.inLeft <> i2
////        |  check.inRight <> i1
////        |  o <> check.outResult
////        |}
////        |
////        |val top_ioDesignConn4 = new IODesignConn4 {}
////      """.stripMargin
////    top_ioDesignConn4.codeString =@= compare
////  }
//
//  property("IODesignMatch.codeString") = {
//    import DFDesign.allowTop._
//    implicit val config = DFAnyConfiguration.detailed
//    val top_ioDesignMatch = new IODesignMatch {}
//    val compare =
//      """
//        |trait IODesignMatch extends DFDesign {
//        |  val i1 = DFUInt(8) <> IN init(1, 1, ?, 1)                  //init = (1, 1, ?, 1)
//        |  val i2 = DFUInt(8) <> IN init(2, 8, 7, 11, 21)             //init = (2, 8, 7, 11, 21)
//        |  val o1 = DFUInt(8) <> OUT                                  //init = ()
//        |  matchdf(i2, MatchConfig.AllowOverlappingCases)
//        |  .casedf(1 to 5, 10 to 20) {
//        |    o1 := i1
//        |  }.casedf(7) {
//        |    o1 := i2
//        |  }.casedf(11) {
//        |    o1 := i2
//        |  }.casedf_ {
//        |    o1 := i2
//        |  }
//        |  val o2 = DFUInt(8) <> OUT                                  //init = (1, 88, 75, 1, 88)
//        |  val ret = DFUInt(8) init(1, 88, 75, 1, 88)                 //init = (1, 88, 75, 1, 88)
//        |  matchdf(i2)
//        |  .casedf(1 to 5, 10 to 20) {
//        |    ret := i1
//        |  }.casedf(7) {
//        |    ret := 75
//        |  }.casedf_ {
//        |    ret := 88
//        |  }
//        |  o2 <> ret
//        |  val i3 = DFEnum(Foo) <> IN init(Foo.Baz0, Foo.Baz3)        //init = (Foo.Baz0, Foo.Baz3)
//        |  val o3 = DFUInt(8) <> OUT                                  //init = ()
//        |  matchdf(i3)
//        |  .casedf(Foo.Baz0) {
//        |    o3 := 1
//        |  }.casedf(Foo.Baz1) {
//        |    o3 := 0
//        |  }
//        |}
//        |
//        |val top_ioDesignMatch = new IODesignMatch {}
//      """.stripMargin
//    top_ioDesignMatch.codeString =@= compare
//  }
//
//  trait ContainerConnLoop extends DFDesign {
//    val i = DFUInt(8) <> IN
//    val o = DFUInt(8) <> OUT
//    val io = new IODesignConn1 {}
//    io.i <> io.o
//    o <> io.o
//  }
//
//  property("ContainerConnLoop exception") = {
//    import DFDesign.allowTop._
//    val topLoop = new ContainerConnLoop {}
//    val expectedError =
//      """
//        |A cyclic connectivity loop detected
//        |topLoop.io.o <> topLoop.io.i
//      """.stripMargin
//    illRunCompare(expectedError) {
//      topLoop.codeString
//    }
//  }
//
//}
