package DFiant
package compiler.backend.vhdl

private object EnumTypeDcl {
  def apply(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    val typeList = enumType.entries.toList.sortBy(x => x._1).map(x => s"E_${enumType.name}_${x._2.name}".toUpperCase)
    s"$KW type ${enumType.name}_type $KW is (${typeList.mkString(", ")});"
  }
}