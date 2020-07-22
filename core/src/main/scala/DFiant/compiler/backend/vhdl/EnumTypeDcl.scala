package DFiant
package compiler.backend.vhdl

private object EnumTypeDcl {
  def enumTypeName(enumType: EnumType)(implicit printer : Printer) : String =
    s"E_${enumType.name}"
  def enumEntryFullName(entry : EnumType.Entry)(implicit printer: Printer) : String =
    s"${enumTypeName(entry.enumType)}_${entry.name}"
  def apply(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    val typeList = enumType.entries.toList.sortBy(x => x._1).map(x => EnumTypeDcl.enumEntryFullName(x._2))
    s"$KW type ${enumTypeName(enumType)} $KW is (${typeList.mkString(", ")});"
  }
}