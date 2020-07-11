package DFiant
package compiler.backend.verilog
import printer.formatter._

object GlobalDefsFile {
  def apply()(implicit printer: Printer) : String = {
    import printer.config._
    val name = Name()
    val defName = s"${name.toUpperCase}_H"
    s"""$KW`ifndef $defName
       |$KW`define $defName
       |${enumDcl.delim()}
       |$KW`endif""".stripMargin.formatted
  }
  def Name()(implicit printer: Printer) : String = s"${printer.getSet.designDB.top.designType}_defs"
//  def assertMacro(implicit printer: Printer) : String =  {
//    s"""
//       |`define assert(cond, msg) \
//       |    if (!cond) begin \
//       |        $$display("ASSERTION FAILED in %m: signal != value"); \
//       |    end
//       |""".stripMargin
//  }
  private def enumDcl(implicit printer: Printer) : String =
    printer.getSet.designDB.getGlobalEnumTypes.map(e => EnumTypeDcl(e)).mkString("\n")
}
