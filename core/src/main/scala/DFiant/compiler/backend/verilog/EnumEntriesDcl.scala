package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object EnumEntriesDcl {
  def defines(entries: DFEnum.Entries)(implicit printer : Printer) : String = {
    import printer.config._
    entries.all.toList.sortBy(x => x._1).map {x =>
      s"$FN`define ${enumEntryFullName(x._2)} ${ALGN(0)}${x._1}"
    }.mkString("\n")
  }
  def moduleName(entries: DFEnum.Entries)(implicit printer : Printer) : String = {
    s"E_${entries.name}"
  }
  def enumEntryFullName(entry : DFEnum.Entries.Entry)(implicit printer: Printer) : String =
    s"${moduleName(entry.entries)}_${entry.name}"
  def enumEntryRefName(entry : DFEnum.Entries.Entry)(implicit printer: Printer) : String =
    s"`${enumEntryFullName(entry)}"
  def tostrFuncName(entries: DFEnum.Entries)(implicit printer: Printer) : String =
    s"${moduleName(entries)}.tostr"
  private def tostrFuncDcl(entries: DFEnum.Entries)(implicit printer: Printer) : String = {
    import printer.config._
    val longestName = entries.all.values.map(v => enumEntryFullName(v).length).max
    val caseItems : List[String] = entries.all.map {
      case (n, e) => Case.Item(s"$LIT ${n}", List(s"""tostr = "${enumEntryFullName(e)}";"""))
    }.toList :+ Case.Item(Case.Choice.Default(), List(s"""tostr = "XXXX";"""))
    s"""$KW function [$LIT ${longestName}$OP*$LIT 8:$LIT 1] tostr ($KW input [$LIT ${entries.width-1}:$LIT 0] e);
       |$KW begin
       |${Case("e", caseItems.mkString("\n"), allowDontCare = false).delim()}
       |$KW end
       |$KW endfunction
       |""".stripMargin
  }
  private def module(entries: DFEnum.Entries)(implicit printer: Printer) : String =
    Module(moduleName(entries), Nil, List(tostrFuncDcl(entries)))
  def apply(entries: DFEnum.Entries)(implicit printer: Printer) : String =
    s"""/* verilator lint_off DECLFILENAME */
       |${module(entries)}""".stripMargin

}

private object EnumInstance {
  def apply(entries: DFEnum.Entries)(implicit printer: Printer) : String =
    s"${EnumEntriesDcl.moduleName(entries)} ${EnumEntriesDcl.moduleName(entries)}();"
}