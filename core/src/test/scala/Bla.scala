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
  val eight = 8
  val b8 = DFBits(eight)
  val a: Inlined.Int[Int] = b8.width
  // val z = MyFields | MyFields2 | MyFields
  // Color.getClass.getFields.map(print)
  // val x: core.DFType.DFVector[DFBit, Tuple1[5]] = DFBit.X(5)
  // // Color.foo
  // import core.DFType
  // val x: DFType.DFTuple[(DFBit, DFBits[8], DFBool)] =
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
  //   (DFBit, DFBits[8], DFBool).dfType
}
