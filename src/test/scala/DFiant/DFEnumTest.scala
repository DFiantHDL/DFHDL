import DFiant._

import org.scalacheck._
import shapeless.test.illTyped
import singleton.ops._
import TestUtils._

class DFEnumAutoTest extends Properties("DFEnumAutoTest") {
  object Foo extends Enum.Auto {
    val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
  }

  object GreyFoo extends Enum.Auto(Enum.Encoding.Grey) {
    val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
  }

  object OneHotFoo extends Enum.Auto(Enum.Encoding.OneHot) {
    val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
  }

  object StartAt25Foo extends Enum.Auto(Enum.Encoding.StartAt(25)) {
    val Baz0, Baz1, Baz2, Baz3, Baz4 = Entry
  }


  trait MyDesign extends DFDesign {
    val p1 = DFEnum(Foo) <> IN
    val p2 = DFEnum(Foo) <> IN init Foo.Baz2
    val p3 = DFEnum(Foo) <> OUT
    p3 := DFEnumFromEntry(Foo.Baz1)
    val e = DFEnum(Foo)
    val p4 = DFEnum(Foo) <> IN
    val p5 = DFEnum(Foo) <> OUT
    e := Foo.Baz1
    e == Foo.Baz2
    e.bits(1,0)
    illRun {e.bits(4,0)}
  }

  property("MyDesign") = {
    import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
    val myDesign = new MyDesign {}
    true
  }

  property("Foo.codeString") = {
    val compare =
      """
        |object Foo extends Enum.Auto {
        |  val Baz0            = Entry()  //0
        |  val Baz1            = Entry()  //1
        |  val Baz2            = Entry()  //2
        |  val Baz3            = Entry()  //3
        |  val Baz4            = Entry()  //4
        |}
      """.stripMargin
    Foo.codeString =@= compare
  }

  property("GreyFoo.codeString") = {
    val compare =
      """
        |object GreyFoo extends Enum.Auto(Enum.Encoding.Grey) {
        |  val Baz0            = Entry()  //0
        |  val Baz1            = Entry()  //1
        |  val Baz2            = Entry()  //3
        |  val Baz3            = Entry()  //2
        |  val Baz4            = Entry()  //6
        |}
      """.stripMargin
    GreyFoo.codeString =@= compare
  }

  property("OneHotFoo.codeString") = {
    val compare =
      """
        |object OneHotFoo extends Enum.Auto(Enum.Encoding.OneHot) {
        |  val Baz0            = Entry()  //1
        |  val Baz1            = Entry()  //2
        |  val Baz2            = Entry()  //4
        |  val Baz3            = Entry()  //8
        |  val Baz4            = Entry()  //16
        |}
      """.stripMargin
    OneHotFoo.codeString =@= compare
  }

  property("StartAt25Foo.codeString") = {
    val compare =
      """
        |object StartAt25Foo extends Enum.Auto(Enum.Encoding.StartAt(25)) {
        |  val Baz0            = Entry()  //25
        |  val Baz1            = Entry()  //26
        |  val Baz2            = Entry()  //27
        |  val Baz3            = Entry()  //28
        |  val Baz4            = Entry()  //29
        |}
      """.stripMargin
    StartAt25Foo.codeString =@= compare
  }
}


class DFEnumManualTest extends Properties("DFEnumManualTest") {
  object Foo extends Enum.Manual(2) {
    val Bar0 = Entry(0)
    val Bar1 = Entry(1)
    val Bar2 = EntryIncLastBy(1)
    val Bar3 = Entry(b"11")
    illRun {val Bar0a = Entry(0L)}
    illRun {val Bar0b = Entry(BigInt(0))}
    illRun {val Bar4 = EntryIncLastBy(1)}
  }

  trait MyDesign extends DFDesign {
    val e = DFEnum(Foo)
    val f = e.init(Foo.Bar1, Foo.Bar2)
    implicitly[Require[f.Width == 2]]
    f := Foo.Bar1
    f == Foo.Bar2
    f.prev.bits(1,0)
    illTyped("""f.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }

  property("MyDesign") = {
    import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
    val myDesign = new MyDesign {}
    true
  }

  property("Foo.codeString") = {
    val compare =
      """
        |object Foo extends Enum.Manual(2) {
        |  val Bar0            = Entry(b"00")
        |  val Bar1            = Entry(b"01")
        |  val Bar2            = Entry(b"10")
        |  val Bar3            = Entry(b"11")
        |}
      """.stripMargin
    Foo.codeString =@= compare
  }
}
