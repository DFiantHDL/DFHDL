package DFiant

import org.scalacheck._
import shapeless.test.illTyped

class DFComponentTest extends Properties("DFComponentTest") {
  trait Adder[Left <: DFAny, Right <: DFAny, Result <: DFAny] extends DFComponent[Adder[Left, Right, Result]] {
    val left : Left <> IN
    val right : Right <> IN
    val result : Result <> OUT
  }
  object Adder {
    type DFU[LW, RW, OW] = Adder[DFUInt[LW], DFUInt[RW], DFUInt[OW]]
    implicit def fro[LW, RW, OW] : DFComponent.Implementation[Adder.DFU[LW, RW, OW]] = ifc => {
      import ifc._
      result := left + right
    }
  }

}
