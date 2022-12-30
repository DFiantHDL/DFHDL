package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern

protected trait VerilogOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: VerilogPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "v"
  def defsName: String =
    s"${getSet.designDB.top.dclName}_defs"
  def csLibrary(inSimulation: Boolean): String =
    s"""`default_nettype none
       |`timescale 1ns/1ps
       |`include "$defsName.v"""".stripMargin
  def moduleName(design: DFDesignBlock): String = design.dclName
  def csModuleDcl(design: DFDesignBlock): String =
    val ports = design
      .members(MemberView.Folded)
      .view
      .collect {
        case p: DFVal.Dcl if p.isPort => printer.csDFValNamed(p)
      }
      .mkString(",\n")
    val portBlock = ports.emptyOr(v => s"""(
         |${ports.indent}
         |);""".stripMargin)
    val localTypeDcls = printer.csLocalTypeDcls(design)
    val designMembers = design.members(MemberView.Folded)
    val dfValDcls =
      designMembers.view
        .collect {
          case p: DFVal.Dcl if p.isVar          => p
          case c: DFVal.Const if !c.isAnonymous => c
        }
        .map(printer.csDFValNamed)
        .emptyOr(_.mkString("\n"))
    val declarations = s"$localTypeDcls$dfValDcls".emptyOr(v => s"\n${v.indent}")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl   => false
      case _: DFVal.Const => false
      case _              => true
    })
    s"""module ${moduleName(design)}$portBlock$declarations
       |${statements.indent}
       |endmodule""".stripMargin
  end csModuleDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csModuleDcl(design)}""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerLateBody(design)
    val inst = s"${moduleName(design)} ${design.name}"
    if (body.isEmpty) s"$inst" else s"$inst(\n${body.indent}\n);"
  def csDFIfStatement(csCond: String): String = s"if ($csCond)"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"else if ($csCond)"
  def csDFIfEnd: String = ""
  def csIfBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "others"
  def csDFCasePatternAlternativeToken: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String = printer.unsupported
  def csDFCasePatternBind(pattern: Pattern.Bind): String = printer.unsupported
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String = printer.unsupported
  def csDFCaseKeyword: String = ""
  def csDFCaseSeparator: String = ":"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String): String = s"case ($csSelector)"
  def csDFMatchEnd: String = "endcase"
  def csProcessBlock(pb: ProcessBlock): String =
    val (statements, dcls) = pb
      .members(MemberView.Folded)
      .partition {
        case dcl: DFVal.Dcl                           => false
        case const: DFVal.Const if !const.isAnonymous => false
        case _                                        => true
      }
    val body = csDFMembers(statements)
    val dcl =
      if (dcls.isEmpty) ""
      else s"${csDFMembers(dcls)}\n"
    val named = pb.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All => " @(*)"
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" @${refs.map(_.refCodeString).mkStringBrackets}"
    s"$dcl${named}always$senList\nbegin\n${body.indent}\nend"
  end csProcessBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VerilogOwnerPrinter
