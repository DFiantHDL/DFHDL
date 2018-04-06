package DFiant

import org.scalacheck._
import shapeless.test.illTyped
import psuedoVendor.family.device._

class DFEnumTest extends Properties("DFEnumTest") {
  sealed trait Foo extends DFEnum.Auto
  case object Bar1 extends Foo
  case object Bar2 extends Foo


  trait MyDesign extends DFDesign {
    val e = DFEnum[Foo]
    e := Bar1
    e == Bar2
    e.bits(1,0)
    illTyped("""e.bits(3,0)""", "Bit index 3 is out of range of width 2")
  }

}
