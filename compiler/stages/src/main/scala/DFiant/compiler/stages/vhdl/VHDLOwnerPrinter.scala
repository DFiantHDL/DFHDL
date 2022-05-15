package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern

protected trait VHDLOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: VHDLPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "vhdl"
  def packageName: String =
    s"${getSet.designDB.top.dclName}_pkg"
  def csLibrary(inSimulation: Boolean): String =
    val default =
      s"""library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.$packageName.all;""".stripMargin
    if (useStdSimLibrary && inSimulation)
      s"""$default
         |
         |library std;
         |use std.env.all;""".stripMargin
    else default
  def entityName(design: DFDesignBlock): String = design.dclName
  def csEntityDcl(design: DFDesignBlock): String =
    val ports = design
      .members(MemberView.Folded)
      .view
      .collect {
        case p: DFVal.Dcl if p.isPort => printer.csDFValNamed(p)
      }
      .mkString(";\n")
    val portBlock = ports.emptyOr(v => s"""
         |port (
         |${ports.indent}
         |);""".stripMargin)
    s"""entity ${entityName(design)} is$portBlock
       |end ${entityName(design)};""".stripMargin
  def archName(design: DFDesignBlock): String = s"${design.dclName}_arch"
  def csArchitectureDcl(design: DFDesignBlock): String =
    val localTypeDcls = printer.csLocalTypeDcls(design)
    val designMembers = design.members(MemberView.Folded)
    val dfValDcls =
      designMembers.view
        .collect {
          case p: DFVal.Dcl if p.isVar          => p
          case c: DFVal.Const if !c.isAnonymous => c
        }
        .map(printer.csDFValNamed)
        .emptyOr(_.mkString("", ";\n", ";"))
    val declarations = s"$localTypeDcls$dfValDcls".emptyOr(v => s"\n${v.indent}")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl   => false
      case _: DFVal.Const => false
      case _              => true
    })
    s"""architecture ${archName(design)} of ${design.dclName} is$declarations
       |begin
       |${statements.indent}
       |end ${archName(design)};""".stripMargin
  end csArchitectureDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csEntityDcl(design)}
       |
       |${csArchitectureDcl(design)}""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerLateBody(design)
    val inst = s"${design.name} : entity work.${entityName(design)}(${archName(design)})"
    if (body.isEmpty) s"$inst" else s"$inst port map (\n${body.indent}\n)"
  def csDFIfStatement(csCond: String): String = s"if $csCond then"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"elsif $csCond then"
  def csDFIfEnd: String = "end if"
  def csIfBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "others"
  def csDFCasePatternAlternativeToken: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String = printer.unsupported
  def csDFCasePatternBind(pattern: Pattern.Bind): String = printer.unsupported
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String = printer.unsupported
  def csDFCaseKeyword: String = "when"
  def csDFCaseSeparator: String = "=>"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String): String = s"case $csSelector is"
  def csDFMatchEnd: String = "end case"
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
      else s"\n${csDFMembers(dcls).indent}"
    val named = pb.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All => " (all)"
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" ${refs.map(_.refCodeString).mkStringBrackets}"
    s"${named}process$senList$dcl\nbegin\n${body.indent}\nend process"
  end csProcessBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VHDLOwnerPrinter
