import DFiant.*
import internals.{MetaContext, Inlined}
import DFiant.core.DFType.DFEncoding
given MetaContext = ???

enum Color extends DFEncoding.Default:
  case Red, Green, Blue

case object MyFields extends DFFields:
  val x = DFBits(8)
  val y = DFBit

case object MyFields2 extends DFFields:
  val x = DFBit
  val y = DFBits(8)

object Bla {
  val z = MyFields | MyFields2 | MyFields

  // Color.foo
  Color.dfType
}
