package course

import DFiant._

trait Mux1 extends DFDesign {
  final val sel = DFBool() <> IN
  final val a   = DFBool() <> IN
  final val b   = DFBool() <> IN
  final val res = DFBool() <> OUT
  res := ((sel && a) || (!sel && b))
}

trait MuxN extends DFDesign {
  val n = 32
  final val sel = DFBool() <> IN
  final val a   = DFBits(n) <> IN
  final val b   = DFBits(n) <> IN
  final val res = DFBits(n) <> OUT
  for (i <- 0 until n) {
    val mux = new Mux1 {}.setName(s"m$i")
    mux.sel <> sel
    mux.a <> a(i)
    mux.b <> b(i)
    res(i) := mux.res
  }
}

//trait RightShifter extends DFDesign {
//  final val vec   = DFBits(32) <> IN
//  final val shift = DFUInt(5)  <> IN
//  final val res   = DFBits(32) <> OUT
//  private val temp = DFBits(32)
//  temp := vec
//  for (i <- 4 to 0 by -1) {
//    val mux = new MuxN{}.setName(s"m$i")
//    mux.a <> (temp >> (1 << i))
//    mux.b <> temp
//    mux.sel <> shift.bit(i)
//    temp := mux.res
//  }
//  res := temp
//}

trait RightShifter extends DFDesign {
  final val vec   = DFBits(32) <> IN
  final val shift = DFUInt(5)  <> IN
  final val res   = DFBits(32) <> OUT
  //res:=vec>>shift
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  private val mux2shift1=new MuxN {}.setName(s"shift1")
  private val mux2shift2=new MuxN {}.setName(s"shift2")
  private val mux2shift4=new MuxN {}.setName(s"shift4")
  private val mux2shift8=new MuxN {}.setName(s"shift8")
  private val mux2shift16=new MuxN {}.setName(s"shift16")
  mux2shift1.sel<>shift.bits(0, 0)
  mux2shift2.sel<>shift.bits(1, 1)
  mux2shift4.sel<>shift.bits(2, 2)
  mux2shift8.sel<>shift.bits(3, 3)
  mux2shift16.sel<>shift.bits(4, 4)
  mux2shift1.b<>vec
  mux2shift2.b<>mux2shift1.res
  mux2shift4.b<>mux2shift2.res
  mux2shift8.b<>mux2shift4.res
  mux2shift16.b<>mux2shift8.res
  res<>mux2shift16.res
  private val shifted1=vec>>1
  private val shifted2=mux2shift1.res>>2
  private val shifted4=mux2shift2.res>>4
  private val shifted8=mux2shift4.res>>8
  private val shifted16=mux2shift8.res>>16
  mux2shift1.a<>shifted1
  mux2shift2.a<>shifted2
  mux2shift4.a<>shifted4
  mux2shift8.a<>shifted8
  mux2shift16.a<>shifted16
  val temp = DFUInt(1).keep
  temp := temp
  /*private val mux2shift = Array.fill(5)(new MuxN {})
  val mux1=mux2shift(0).setName(s"shift1")
  val mux2=mux2shift(1).setName(s"shift2")
  val mux4=mux2shift(2).setName(s"shift4")
  val mux8=mux2shift(3).setName(s"shift8")
  val mux16=mux2shift(4).setName(s"shift16")
  private val mux1_t = new MuxN {}
  private val mux2 = new MuxN {}
  private val mux4 = new MuxN {}
  private val mux8 = new MuxN {}
  private val mux16 = new MuxN {}
  mux1_t.sel<>shift.bits(0,0)
  mux2.sel<>shift.bits(1,1)
  mux4.sel<>shift.bits(2,2)
  mux8.sel<>shift.bits(3,3)
  mux16.sel<>shift.bits(4,4)
  mux1_t.b<>vec
  mux2.b<>mux1_t.res
  mux4.b<>mux2.res
  mux8.b<>mux4.res
  mux16.b<>mux8.res
  res<>mux16.res
  private val shiftRes1 = (vec>>1)
  private val shiftRes2 = ((mux1_t.res)>>2)
  private val shiftRes4 = ((mux2.res)>>4)
  private val shiftRes8 = ((mux4.res)>>8)
  private val shiftRes16 = ((mux8.res)>>16)
  mux1_t.a<>(vec>>1)
  mux2.a<>((mux1_t.res)>>2)
  mux4.a<>((mux2.res)>>4)
  mux8.a<>((mux4.res)>>8)
  mux16.a<>((mux8.res)>>16)*/
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

trait RightShifter_TB extends DFSimulator {
  private val rightShifter = new RightShifter{}

  private val testCases = Seq(         //This is a sequence of Tuple3
    //(vec        , shift, expected   )
    (h"80000000", 4    , h"08000000"),
    (h"80000000", 1    , h"40000000")
  ).reverse //initialization of init will be bottom to top
  private val testNum = testCases.length
  private val vecSeq = testCases.map(t => t._1)     //getting just the vec test values
  private val shiftSeq = testCases.map(t => t._2)   //getting just the shift test values
  private val expectedSeq = testCases.map(t => t._3)//getting just the expected test values

  private val vec = DFBits(32) init vecSeq
  vec := vec.prev(testNum)
  private val shift = DFUInt(5) init shiftSeq
  shift := shift.prev(testNum)
  private val expected = DFBits(32) init expectedSeq
  expected := expected.prev(testNum)

  rightShifter.vec <> vec
  rightShifter.shift <> shift
  sim.assert(rightShifter.res == expected, msg"expected $vec >> $shift = $expected, but got ${rightShifter.res}")
}


object Lab1 extends App {
  println("Hello world! I'm Lab #1")
  val rightShifter_tb = new RightShifter_TB {}.compileToVHDL.print().toFile("lab1.vhd")
}
