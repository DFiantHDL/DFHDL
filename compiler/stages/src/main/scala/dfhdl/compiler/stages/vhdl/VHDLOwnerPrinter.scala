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
    val designMembers = design.members(MemberView.Folded)
    val ports = designMembers.view
      .collect { case p @ DclPort() =>
        printer.csDFMember(p)
      }
      .mkString(";\n")
    val designParamList = designMembers.collect { case param @ DesignParam(_) =>
      val defaultValue = if (design.isTop) s" := ${param.relValRef.refCodeString}" else ""
      s"${param.getName} : ${printer.csDFType(param.dfType)}$defaultValue"
    }
    val genericBlock =
      if (designParamList.length == 0) ""
      else "\ngeneric (" + designParamList.mkString("\n", ";\n", "\n").hindent(1) + ");"
    val portBlock = ports.emptyOr(v => s"""
         |port (
         |${ports.hindent}
         |);""".stripMargin)
    s"""entity ${entityName(design)} is$genericBlock$portBlock
       |end ${entityName(design)};""".stripMargin
  end csEntityDcl
  def archName(design: DFDesignBlock): String = s"${design.dclName}_arch"
  def csArchitectureDcl(design: DFDesignBlock): String =
    val localTypeDcls = printer.csLocalTypeDcls(design)
    val designMembers = design.members(MemberView.Folded)
    val dfValDcls =
      designMembers.view
        .flatMap {
          case p: DFVal.Dcl if p.isVar => Some(p)
          case DesignParam(_)          => None
          case c @ DclConst()          => Some(c)
          case _                       => None
        }
        .map(printer.csDFMember)
        .toList
        .emptyOr(_.mkString("\n"))
    val declarations =
      s"${localTypeDcls.emptyOr(x => s"$x\n")}$dfValDcls".emptyOr(v => s"\n${v.hindent}")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl => false
      case DclConst()   => false
      case _            => true
    })
    s"""architecture ${archName(design)} of ${design.dclName} is$declarations
       |begin
       |${statements.hindent}
       |end ${archName(design)};""".stripMargin
  end csArchitectureDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csEntityDcl(design)}
       |
       |${csArchitectureDcl(design)}
       |""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFDesignLateBody(design)
    val designParamList = design.members(MemberView.Folded).collect { case param @ DesignParam(_) =>
      s"${param.getName} => ${param.relValRef.refCodeString}"
    }
    val designParamCS =
      if (designParamList.isEmpty) ""
      else " generic map (" + designParamList.mkString("\n", ",\n", "\n").hindent(1) + ")"
    val inst =
      s"${design.getName} : entity work.${entityName(design)}(${archName(design)})${designParamCS}"
    if (body.isEmpty) s"$inst;" else s"$inst port map (\n${body.hindent}\n);"
  def csDFDesignDefDcl(design: DFDesignBlock): String = printer.unsupported
  def csDFDesignDefInst(design: DFDesignBlock): String = printer.unsupported
  def csBlockBegin: String = ""
  def csBlockEnd: String = ""
  def csDFIfStatement(csCond: String): String = s"if $csCond then"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"elsif $csCond then"
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String = "end if;"
  def csIfBlockEmpty: String = ""
  def csDFCaseBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "others"
  def csDFCasePatternAlternativeData: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String = printer.unsupported
  def csDFCasePatternBind(pattern: Pattern.Bind): String = printer.unsupported
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String = printer.unsupported
  def csDFCaseKeyword: String = "when "
  def csDFCaseSeparator: String = " =>"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String): String = s"case $csSelector is"
  def csDFMatchEnd: String = "end case;"
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
      else s"\n${csDFMembers(dcls).hindent}"
    val named = pb.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All => " (all)"
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" ${refs.map(_.refCodeString).mkStringBrackets}"
    s"${named}process$senList$dcl\nbegin\n${body.hindent}\nend process;"
  end csProcessBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VHDLOwnerPrinter
