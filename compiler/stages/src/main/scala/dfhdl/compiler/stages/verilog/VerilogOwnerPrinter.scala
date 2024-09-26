package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFVal.Func.Op as FuncOp

protected trait VerilogOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: VerilogPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "v"
  def defsName: String =
    s"${getSet.designDB.top.dclName}_defs"
  def csLibrary(inSimulation: Boolean): String =
    s"""`default_nettype none
       |`timescale 1ns/1ps
       |`include "${printer.globalFileName}"""".stripMargin
  def moduleName(design: DFDesignBlock): String = design.dclName
  val parameterizedModuleSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 => false
      case _                  => true
  def csModuleDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    val ports = designMembers.view.collect { case p @ DclPort() =>
      if (parameterizedModuleSupport) printer.csDFMember(p)
      else p.getName
    }
      .mkString(",\n")
    val portBlock = ports.emptyOr(v => s"""(
                                          |${ports.hindent}
                                          |)""".stripMargin)
    val localTypeDcls = printer.csLocalTypeDcls(design).emptyOr(x => s"$x\n")
    val dfValDcls =
      designMembers.view
        .flatMap {
          case p: DFVal.Dcl if p.isVar || !parameterizedModuleSupport => Some(p)
          case p @ DesignParam(_) =>
            if (parameterizedModuleSupport) None
            else Some(p)
          case c @ DclConst() => Some(c)
          case _              => None
        }
        .map(x => printer.csDFMember(x) + ";")
        .toList
        .emptyOr(_.mkString("\n"))
    val declarations = s"$localTypeDcls$dfValDcls".emptyOr(v => s"\n${v.hindent}")
    val statements = csDFMembers(
      designMembers.filter {
        case _: DFVal.Dcl => false
        case DclConst()   => false
        case _            => true
      }
    )
    val designParamList = designMembers.collect { case param @ DesignParam(_) =>
      val defaultValue = if (design.isTop) s" = ${param.relValRef.refCodeString}" else ""
      s"parameter ${printer.csDFType(param.dfType)} ${param.getName}$defaultValue"
    }
    val designParamCS =
      if (designParamList.length == 0 || !parameterizedModuleSupport) ""
      else if (designParamList.length == 1) designParamList.mkString("#(", ", ", ")")
      else "#(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    s"""module ${moduleName(design)}$designParamCS$portBlock;
       |  `include "dfhdl_defs.${printer.verilogFileHeaderSuffix}"$declarations
       |${statements.hindent}
       |endmodule""".stripMargin
  end csModuleDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csModuleDcl(design)}
       |""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFDesignLateBody(design)
    val designParamList = design.members(MemberView.Folded).collect { case param @ DesignParam(_) =>
      s".${param.getName} (${param.relValRef.refCodeString})"
    }
    val designParamCS =
      if (designParamList.isEmpty) ""
      else " #(" + designParamList.mkString("\n", ",\n", "\n").hindent(1) + ")"
    val inst = s"${moduleName(design)}$designParamCS ${design.getName}"
    if (body.isEmpty) s"$inst;" else s"$inst(\n${body.hindent}\n);"
  def csDFDesignDefDcl(design: DFDesignBlock): String = printer.unsupported
  def csDFDesignDefInst(design: DFDesignBlock): String = printer.unsupported
  def csBlockBegin: String = "begin"
  def csBlockEnd: String = "end"
  def csDFIfStatement(csCond: String): String = s"if ($csCond)"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"else if ($csCond)"
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String = ""
  def csIfBlockEmpty: String = "begin end"
  def csDFCaseBlockEmpty: String = "begin end"
  def csDFCasePatternCatchAll: String = "default"
  def csDFCasePatternAlternativeData: String = ", "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String = printer.unsupported
  def csDFCasePatternBind(pattern: Pattern.Bind): String = printer.unsupported
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String = printer.unsupported
  def csDFCaseKeyword: String = ""
  def csDFCaseSeparator: String = ":"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean): String =
    val insideSupport = printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => false
      case _                                         => true
    val keyWord = if (wildcardSupport && !insideSupport) "casez" else "case"
    val insideStr = if (wildcardSupport && insideSupport) " inside" else ""
    s"$keyWord ($csSelector)$insideStr"
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
    val alwaysKW = printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => "always"
      case _ =>
        pb.sensitivity match
          case Sensitivity.All => "always_comb"
          case Sensitivity.List(refs) =>
            refs match
              case DFRef(DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _)) :: Nil =>
                "always_ff"
              case DFRef(DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _)) ::
                  DFRef(DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _)) :: Nil =>
                "always_ff"
              case _ => "always"
    val senList = pb.sensitivity match
      case Sensitivity.All => if (alwaysKW == "always") " @(*)" else ""
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" @${refs.map(_.refCodeString).mkStringBrackets}"
    s"$dcl${named}$alwaysKW$senList\nbegin\n${body.hindent}\nend"
  end csProcessBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VerilogOwnerPrinter
