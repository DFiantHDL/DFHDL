import DFiant.*
import internals.*
enum Color extends DFEncoding.OneHot:
  case Red, Green, Blue

given MetaContext = ???

object Bla {
  val eight = 8
  val b8 = DFBits(8)
  val b8b = DFBits(8)
  val b8c = DFBits(8)
  val x: DFBits[8] = b8
  val a: Inlined.Int[80] = b8.X(10).width
  val o1 = b8.opaque
  val o2 = b8.opaque
  b8 | o1 | o2
//  val b: Inlined.Int[11] = (Color, b8).width
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
