package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import scodec.bits._

class DFEnumAutoTest extends Properties("DFEnumAutoTest") {
  object Foo extends Enum.Auto {
    sealed class Entry extends Enum.Auto.Entry
    case object Bar1 extends Entry
    case object Bar2 extends Entry
  }

  trait MyDesign extends DFDesign {
    val e = Foo.DFEnum()
    e := Foo.Bar1
    e == Foo.Bar2
    e.bits(1,0)
    illTyped("""e.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }
}


class DFEnumManualTest extends Properties("DFEnumManualTest") {
  //  sealed class Foo(value : BitVector) extends DFEnum.Manual[4]
  //  case object Bar1 extends Foo(bin"0001")
  //  case object Bar2 extends Foo(bin"0010")
  object Foo extends Enum.Auto {
    sealed class Entry extends Enum.Auto.Entry
    case object Bar1 extends Entry
    case object Bar2 extends Entry
  }
  //
  //  object Foo extends DFEnum.Manual[4] {
  //    case object Bar1 extends Entry(bin"0001")
  //    case object Bar2 extends Entry(bin"0001")
  //  }
  //  sealed trait Foo extends DFEnum.Auto
  //  case object Bar1 extends Foo
  //  case object Bar2 extends Foo


  trait MyDesign extends DFDesign {
    //    implicitly[DFEnum.Manual[4]]
    //    DFEnum.Builder.manual[4, Foo]
    val e = Foo.DFEnum()
    e := Foo.Bar1
    e == Foo.Bar2
    e.bits(1,0)
    //    illTyped("""e.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }

}
