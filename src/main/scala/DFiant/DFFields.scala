package DFiant

import internals._

abstract class DFFields(implicit n : NameIt) extends HasCodeString with Product {
  final protected implicit val emptyDesign : DFDesign = new DFDesign() {}
  final override def productArity: Int = emptyDesign.memberList.length
  final override def productElement(n: Int): Any = emptyDesign.memberList(n)
  final override def canEqual(that: Any): Boolean = that.isInstanceOf[DFFields]
  final lazy val members : List[DFAny] = emptyDesign.memberList.collect{case x : DFAny => x}
  final val name : String = n.value
  override def toString: String = name
  private val fieldsCodeString : String = emptyDesign.bodyCodeString
  final def codeString : String = s"\nobject $name extends DFFields {$fieldsCodeString\n}"
}
