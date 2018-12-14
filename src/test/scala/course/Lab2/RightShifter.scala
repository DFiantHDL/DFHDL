package course.Lab2

import DFiant._

//All shifter
trait RightShifter extends DFDesign {
  final val w : Int = RightShifter.requestedWidth
  final val vec   = DFBits(w)             <> IN
  final val shift = DFUInt.rangeUntil(w)  <> IN
  final val res   = DFBits(w)             <> OUT
  final val k : Int = shift.width //You may use this value in your loop (e.g if w = 32, k = 5)
}
object RightShifter {
  var requestedWidth : Int = 32 //width of the shifter. Assumed to be power of 2
}

trait RightShifterTester extends DFSimulator {
  val rightShifter : RightShifter
  final val w = rightShifter.w
  final val k = rightShifter.k
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //Add additional test cases here
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  final val testCases = Seq(         //This is a sequence of Tuple3
    //(vec        , shift, expected   )
    (h"80000000", 4    , h"08000000"),
    (h"80000000", 1    , h"40000000")
  ).reverse //initialization of init will be bottom to top

  private val testNum = testCases.length
  private val vecSeq = testCases.map(t => t._1)     //getting just the vec test values
  private val shiftSeq = testCases.map(t => t._2)   //getting just the shift test values
  private val expectedSeq = testCases.map(t => t._3)//getting just the expected test values

  //Cyclic rotation through the test cases
  final val vec = DFBits(w) init vecSeq
  vec := vec.prev(testNum)
  final val shift = DFUInt.rangeUntil(w) init shiftSeq
  shift := shift.prev(testNum)
  final val expected = DFBits(w) init expectedSeq
  expected := expected.prev(testNum)

  rightShifter.vec <> vec
  rightShifter.shift <> shift
  //validating output
  def check() : Unit = {
  }
  check()
  ifdf(rightShifter.asInstanceOf[MulticycleRightShifter].valid){
    val a = rightShifter.res == expected
    a.keep
    //    sim.assert(rightShifter.res == expected, msg"expected $vec >> $shift = $expected, but got ${rightShifter.res}")
  }


}


