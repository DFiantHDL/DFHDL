package DFiant.core
import DFiant._
import DFiant.core.DFUInt.Op.Able
import shapeless.test.illTyped

class DFDesignTest {
  abstract class Box[GenW] extends DFDesign {
    val in1  : DFUInt[GenW] <> IN  = OPEN
    val out1 : DFUInt[GenW] <> OUT = OPEN
    val in2  : DFUInt[GenW] <> IN
    val out2 : DFUInt[GenW] <> OUT = out1
    illTyped("""val out1 : DFUInt[GenW] <> OUT = in1""") //Fail compile mixing input/output ports
    illTyped("""val in2  : DFUInt[GenW] <> IN  = out1""") //Fail compile mixing input/output ports

    out1 := in1 + 1 //can read from input and assign to output
    out2 := out1 //can read from output, but
    illTyped("""in1 := out1""") //Fail (Cannot assign to an input port)

//    abstract class MyInterface[Dir] {
//      val ready : DFBool <> Dir
//      val valid : DFBool <> ![Dir]
//    }
  }

  abstract class Foo extends DFDesign {
    val a = DFUInt(32)
    val in1 : a.TVal <> IN = a
    val out1 : a.TVal <> OUT = a
    val in2 : a.TVal <> IN = a + 1
    illTyped("""val out2 : a.TVal <> OUT = a + 1""")
    illTyped("""val out3 : a.TVal <> OUT = in1""")
  }

  abstract class BoxContainer[GenW] extends DFDesign {
    val in : DFUInt[GenW] <> IN
    val out : DFUInt[GenW] <> OUT
    val box : Box[GenW] = new Box[GenW] {
      override val in1 = in
      val in2 = in
    }
  }

  val top = new DFDesign {


  }

}


object Tomer {
  trait DFAny {
    def isBubble : DFBool = ???
  }

  trait DFBool

  object DFBool {
    implicit def toBoolean(b : DFBool) : Boolean = ???
  }

  case class DFMatrix[DF <: DFAny](rows : Int, cols : Int)(df : DF) {
    trait Element {
      def prevRow(i : Int = 1) : DF = ???
      def nextRow(i : Int = 1) : DF = ???
      def prevCol(i : Int = 1) : DF = ???
      def nextCol(i : Int = 1) : DF = ???
    }
    val element : Element = ???
  }

  trait DFDouble extends DFAny {
    def + (that : DFDouble) : DFDouble = ???
    def - (that : DFDouble) : DFDouble = ???
    def * (that : DFDouble) : DFDouble = ???
    def / (that : DFDouble) : DFDouble = ???
  }

  type Approx[T <: DFAny] = T
}
