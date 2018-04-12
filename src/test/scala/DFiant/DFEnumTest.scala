package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import scodec.bits._
import singleton.ops._

class DFEnumAutoTest extends Properties("DFEnumAutoTest") {
  object Foo extends Enum.Auto(Enum.Encoding.Default) {
    sealed trait Entry extends Enum.Auto.Entry
    val Baz0, Baz1, Baz2, Baz3, Baz4 = new Entry {}
  }

  object NotSealedFoo extends Enum.Auto(Enum.Encoding.Default) {
    class Entry extends Enum.Auto.Entry
    val Baz0, Baz1, Baz2, Baz3, Baz4 = new Entry {}
  }

  object NoEntriesFoo extends Enum.Auto(Enum.Encoding.Default) {
    sealed class Entry extends Enum.Auto.Entry
  }

  trait MyDesign extends DFDesign {
    val p1 : Foo.DFEnum <> IN = OPEN
    val p2 : Foo.DFEnum <> IN = TOP
    val p3 : Foo.DFEnum <> IN = Foo.Baz1
    val e = Foo.DFEnum()
    val p4 : Foo.DFEnum <> IN = e
    val p5 : Foo.DFEnum <> OUT = e
    implicitly[Require[e.Width == 3]]
    e := Foo.Baz1
//    e == Foo.Bar2
    e.bits(1,0)
    illTyped("""e.bits(4,0)""", "Bit index 4 is out of range of width 3")

    illTyped("""NotSealedFoo.DFEnum()""", """No enumeration entries found or the Entry is not a sealed trait""")
    illTyped("""NoEntriesFoo.DFEnum()""", """No enumeration entries found or the Entry is not a sealed trait""")
  }
}


class DFEnumManualTest extends Properties("DFEnumManualTest") {
  object Foo extends Enum.Manual[2] {
    val Bar1 = Entry(1)
    val Bar2 = Entry(0)
    val Bar3 = Entry(0L)
    val Bar4 = Entry(BigInt(0))
    val Bar5 = Entry(bin"11")
  }

  trait MyDesign extends DFDesign {
    val e = Foo.DFEnum()
    val f = e.init(Foo.Bar1, Foo.Bar2)
    implicitly[Require[f.Width == 2]]
    f := Foo.Bar1
//    f == Foo.Bar2
    f.prev.bits(1,0)
    //f.prev(2).prev(2).bits(1,0) TODO: causes compiler crash
    illTyped("""f.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }

}
