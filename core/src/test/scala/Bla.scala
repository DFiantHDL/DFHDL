import DFiant.*
import internals.*
enum Color extends DFEncoding.OneHot:
  case Red, Green, Blue

object Bla {
  val eight = 8
  val b8 = DFBits(8)
  val b8b = DFBits(9)
  val b8c = DFBits(10)
  val x: DFBits[8] = b8
  val a: Inlined.Int[80] = b8.X(10).width
  b8 | b8b | b8c
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
