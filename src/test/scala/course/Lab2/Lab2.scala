package course.Lab2

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
    mux.a <> a(i).pipe
    mux.b <> b(i)
    res(i) := mux.res
  }
}

trait RightShifter extends DFDesign {
  val vec = DFBits(32) <> IN                                 //latency = Some(0)
  val shift = DFUInt(5) <> IN                                //latency = Some(0)
  val res = DFBits(32) <> OUT                                //latency = Some(100)
  val temp = vec
  val m4 = new MuxN {}
  m4.a <> (temp >> 16)
  m4.b <> temp
  m4.sel <> shift.bit(4)
  val temp1 = m4.res.pipe(1)
  val m3 = new MuxN {}
  m3.a <> (temp1 >> 8)
  m3.b <> temp1
  m3.sel <> shift.bit(3)
  val temp2 = m3.res.pipe(1)
  val m2 = new MuxN {}
  m2.a <> (temp2 >> 4)
  m2.b <> temp2
  m2.sel <> shift.bit(2)
  val temp3 = m2.res.pipe(1)
  val m1 = new MuxN {}
  m1.a <> (temp3 >> 2)
  m1.b <> temp3
  m1.sel <> shift.bit(1)
  val temp4 = m1.res.pipe(1)
  val m0 = new MuxN {}
  m0.a <> (temp4 >> 1)
  m0.b <> temp4
  m0.sel <> shift.bit(0)
  val temp5 = m0.res.pipe(1)
  res := temp5

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


object Lab2 extends App {
  println("Hello world! I'm Lab #2")
  implicit val config = DFAnyConfiguration.foldedLatency
  val rightShifter = new RightShifter {}.printCodeString
//  val rightShifter_tb = new RightShifter_TB {}.compileToVHDL.print().toFile("lab2.vhd")
}
