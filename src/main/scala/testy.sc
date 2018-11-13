import DFiant._

trait DFFields {
  final implicit protected val emptyDesign = new DFDesign() {}
}
object Some extends DFFields {
  val a = DFBits(32)
  val b = DFUInt(8)
}

trait DFStruct[S <: DFFields] {
  val fields : S
}

//new Struct(new Some{})