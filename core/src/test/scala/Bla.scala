import DFiant.*
import internals.{MetaContext, Inlined}
given MetaContext = ???

abstract class Line(using MetaContext) extends DFStruct {
  val x = DFBits(8) <> FIELD
  val y = DFBits(8) <> FIELD
}
case object LL extends Line

object Bla {
  // final val z = summon[DFStruct.Fields.Width[Line.type]]
  // summon[z.Out =:= 6]
  val w: Inlined.Int[16] = LL.width
  // summon[DFType.Width[(DFBits[8], DFBits[8])]]
  // val w2: Inlined.Int[16] = ((DFBit, DFBits(7)), DFBits(8)).width
  val w3: Inlined.Int[25] = ((DFBits(8), DFBit), LL).width
  val one = 1
  // val w4: Inlined.Int[100] = ((DFBits(8), DFBit), LL).X(2, 2).width
  // internals.showTree(new Tuple3(7, 1, 5))
}
