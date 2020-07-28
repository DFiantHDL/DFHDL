package DFiant
package compiler.backend.verilog
import printer.formatter._

private object EnumTypeDcl {
  def defines(enumType: EnumType)(implicit printer : Printer) : String = {
    import printer.config._
    enumType.entries.toList.sortBy(x => x._1).map {x =>
      s"$FN`define ${enumEntryFullName(x._2)} ${ALGN(0)}${x._1}"
    }.mkString("\n")
  }
  def moduleName(enumType: EnumType)(implicit printer : Printer) : String = {
    s"E_${enumType.name}"
  }
  def enumEntryFullName(entry : EnumType.Entry)(implicit printer: Printer) : String =
    s"${moduleName(entry.enumType)}_${entry.name}"
  def enumEntryRefName(entry : EnumType.Entry)(implicit printer: Printer) : String =
    s"`${enumEntryFullName(entry)}"
  def tostrFuncName(enumType: EnumType)(implicit printer: Printer) : String =
    s"${moduleName(enumType)}.tostr"
  private def tostrFuncDcl(enumType: EnumType)(implicit printer: Printer) : String = {
    import printer.config._
    val longestName = enumType.entries.values.map(v => enumEntryFullName(v).length).max
    val caseItems : List[String] = enumType.entries.map {
      case (n, e) => Case.Item(s"$LIT ${n}", List(s"""tostr = "${enumEntryFullName(e)}";"""))
    }.toList :+ Case.Item(Case.Choice.Default(), List(s"""tostr = "XXXX";"""))
    s"""$KW function [$LIT ${longestName}$OP*$LIT 8:$LIT 1] tostr ($KW input [$LIT ${enumType.width-1}:$LIT 0] e);
       |$KW begin
       |${Case("e", caseItems.mkString("\n")).delim()}
       |$KW end
       |$KW endfunction
       |""".stripMargin
  }
  private def module(enumType: EnumType)(implicit printer: Printer) : String =
    Module(moduleName(enumType), Nil, List(tostrFuncDcl(enumType)))
  def apply(enumType: EnumType)(implicit printer: Printer) : String =
    s"""/* verilator lint_off DECLFILENAME */
       |${module(enumType)}""".stripMargin

}

private object EnumInstance {
  def apply(enumType: EnumType)(implicit printer: Printer) : String =
    s"${EnumTypeDcl.moduleName(enumType)} ${EnumTypeDcl.moduleName(enumType)}();"
}