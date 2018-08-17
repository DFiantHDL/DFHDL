package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import singleton.ops._
import TestUtils._

class DFEnumAutoTest extends Properties("DFEnumAutoTest") {
  object Foo extends Enum.Auto {
    sealed trait Entry extends Enum.Auto.Entry
    val Baz0, Baz1, Baz2, Baz3, Baz4 = new Entry {}
  }

  object NotSealedFoo extends Enum.Auto {
    class Entry extends Enum.Auto.Entry
    val Baz0, Baz1, Baz2, Baz3, Baz4 = new Entry {}
  }

  object NoEntriesFoo extends Enum.Auto(Enum.Encoding.Default) {
    sealed class Entry extends Enum.Auto.Entry
  }

  trait MyDesign extends DFDesign {
    val p1 = DFEnum(Foo) <> IN
    val p2 = DFEnum(Foo) <> IN init Foo.Baz2
    val p3 = DFEnum(Foo) <> OUT
    p3 := Foo.Baz1
    val e = DFEnum(Foo)
    val p4 = DFEnum(Foo) <> IN
    val p5 = DFEnum(Foo) <> OUT
    implicitly[Require[e.Width == 3]]
    e := Foo.Baz1
    e == Foo.Baz2
    e.bits(1,0)
    illTyped("""e.bits(4,0)""", "Bit index 4 is out of range of width 3")

    illTyped("""DFEnum(NotSealedFoo)""", """No enumeration entries found or the Entry is not a sealed trait""")
    illTyped("""DFEnum(NoEntriesFoo)""", """No enumeration entries found or the Entry is not a sealed trait""")
  }

  property("MyDesign") = {
    import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
    val myDesign = new MyDesign {}
    true
  }
}


class DFEnumManualTest extends Properties("DFEnumManualTest") {
  object Foo extends Enum.Manual(2) {
    val Bar0 = Entry(0)
    val Bar1 = Entry(1)
    val Bar2 = Entry.incLastBy(1)
    val Bar3 = Entry(b"11")
    illRun {val Bar0a = Entry(0L)}
    illRun {val Bar0b = Entry(BigInt(0))}
    illRun {val Bar4 = Entry.incLastBy(1)}
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
}
