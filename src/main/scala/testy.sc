import DFiant._

trait DFFields extends Product {
  final implicit protected val emptyDesign = new DFDesign() {}

  final override def productArity: Int = emptyDesign.memberList.length

  final override def productElement(n: Int): Any = emptyDesign.memberList(n)

  final override def canEqual(that: Any): Boolean = that.isInstanceOf[DFFields]
}
object SomeAB extends DFFields {
  val a = DFBits(32)
  val b = DFUInt(8)
}

trait DFStruct[S <: DFFields] {
  val fields : S
}

//new Struct(new Some{})