package DFiant.core

class DFDesignTest {
  abstract class Box[GenW] extends DFDesign {
    val in1 : DFUInt[GenW] <> IN
    val in2 : DFUInt[GenW] <> IN
    val out1 : DFUInt[GenW] <> OUT
    val out2 : DFUInt[GenW] <> OUT
  }


  abstract class BoxContainer[GenW] extends DFDesign {
    val in : DFUInt[GenW] <> IN
    val out : DFUInt[GenW] <> OUT
    val box : Box[GenW]
  }

  val top = new DFDesign {


  }

}
