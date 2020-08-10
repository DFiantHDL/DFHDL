package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

object GlobalDefsFile {
  def apply()(implicit printer: Printer) : String = {
    import printer.config._
    val name = Name()
    val defName = s"${name.toUpperCase}_H"
    s"""$FN`ifndef $defName
       |$FN`define $defName
       |$enumDcl
       |$FN`endif""".stripMargin.formatted
  }
  def Name()(implicit printer: Printer) : String = s"${printer.getSet.designDB.top.designType}_defs"
  private def enumDcl(implicit printer: Printer) : String =
    printer.getSet.designDB.getGlobalEnumTypes.map(e =>
      s"${EnumTypeDcl.defines(e)}\n${if (printer.inSimulation) EnumTypeDcl(e) else ""}"
    ).mkString("\n")
}
