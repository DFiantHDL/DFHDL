package course

import DFiant._

trait Mux1 extends DFDesign {
  final val sel = DFBool() <> IN
  final val a   = DFBool() <> IN
  final val b   = DFBool() <> IN
  final val res = DFBool() <> OUT
  res := ((sel && a) || (!sel && b))
}

trait Mux8 extends DFDesign {
  final val sel = DFBool() <> IN
  final val a   = DFBits(8) <> IN
  final val b   = DFBits(8) <> IN
  final val res = DFBits(8) <> OUT
  for (i <- 0 until 8) {
    val mux = new Mux1 {}
    mux.sel <> sel
    mux.a <> a(i)
    mux.b <> b(i)
    res(i) := mux.res
  }
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

trait RightShifter extends DFDesign {
  final val vec   = DFBits(32) <> IN
  final val shift = DFUInt(5)  <> IN
  final val res   = DFBits(32) <> OUT
  private val temp = DFBits(32)
  temp := vec
  for (i <- 4 to 0 by -1) {
    val mux = new MuxN{}.setName(s"m$i")
    mux.a <> (temp >> (1 << i))
    mux.b <> temp
    mux.sel <> shift.bit(i)
    temp := mux.res
  }
  res := temp
}

trait RightShifterTest extends DFSimulator {
  private val rightShifter = new RightShifter{}

//  private val testCases = Seq(         //This is a sequence of Tuple3
//    //(vec        , shift, expected   )
//    (h"80000000", 4    , h"08000000"),
//    (h"80000000", 1    , h"40000000")
//  ).reverse //initialization of init will be bottom to top
//  private val testNum = testCases.length
//  private val vecSeq = testCases.map(t => t._1)     //getting just the vec test values
//  private val shiftSeq = testCases.map(t => t._2)   //getting just the shift test values
//  private val expectedSeq = testCases.map(t => t._3)//getting just the expected test values
//
  private val vec = DFBits(32) init b0s
  vec.keep
//  vec := vec.prev(testNum)
//  private val shift = DFUInt(5) init shiftSeq
//  shift := shift.prev(testNum)
//  private val expected = DFBits(32) init expectedSeq
//
  vec := vec.prev << 1
//  rightShifter.shift <> shift
//  sim.assert(rightShifter.res == expected, "Bad result")

}


object Lab1 extends App {
  println("Hello world! I'm Lab #1")

  val rightShifterTest = new RightShifterTest {}.compileToVHDL.print().toFile("test.vhd")

  //  leftShifter.printVHDLString
}
