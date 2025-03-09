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
  val noDefaultParamSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csModuleDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    val ports = designMembers.view.collect { case p @ DclPort() =>
      if (parameterizedModuleSupport) printer.csDFMember(p)
      else p.getName
    }.mkString(",\n")
    val portBlock = ports.emptyOr(v => s"""(
                                          |${ports.hindent}
                                          |)""".stripMargin)
    val localTypeDcls = printer.csLocalTypeDcls(design).emptyOr(x => s"$x\n")
    val constIntDcls =
      designMembers.view
        .flatMap {
          case p: DesignParam =>
            if (parameterizedModuleSupport) None
            else Some(p)
          case c @ DclConst() =>
            c.dfType match
              case DFInt32 => Some(c)
              case _       => None
          case _ => None
        }
        .map(x => printer.csDFMember(x) + ";")
        .toList
        .emptyOr(_.mkString("\n")).emptyOr(x => s"$x\n")
    val dfValDcls =
      designMembers.view
        .flatMap {
          case IteratorDcl()                                          => None
          case p: DFVal.Dcl if p.isVar || !parameterizedModuleSupport => Some(p)
          case _: DesignParam                                         => None
          case c @ DclConst() =>
            c.dfType match
              case DFInt32 => None
              case _       => Some(c)
          case _ => None
        }
        .map(x => printer.csDFMember(x) + ";")
        .toList
        .emptyOr(_.mkString("\n"))
    val declarations = s"$constIntDcls$localTypeDcls$dfValDcls".emptyOr(v => s"\n${v.hindent}")
    val statements = csDFMembers(
      designMembers.filter {
        case _: DFVal.Dcl => false
        case DclConst()   => false
        case _            => true
      }
    )
    val designParamList = designMembers.collect { case param: DesignParam =>
      val defaultValue =
        if (design.isTop) s" = ${param.dfValRef.refCodeString}"
        else
          param.defaultRef.get match
            case DFMember.Empty =>
              // missing default values are supported
              if (noDefaultParamSupport) ""
              // missing default values are not supported, so we fetch a valid constant data
              // (different instances may have different constant data, but for default,
              // a single module description can have any valid data, just to satisfy the standard)
              else s" = ${printer.csConstData(param.dfType, param.getConstData.get)}"
            case _ => s" = ${param.defaultRef.refCodeString}"
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
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      s".${param.getName} (${param.dfValRef.refCodeString})"
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
  val sensitivityListSep =
    printer.dialect match
      case VerilogDialect.v95 => " or "
      case _                  => ", "
  def csProcessBlock(pb: ProcessBlock): String =
    val (statements, dcls) = pb
      .members(MemberView.Folded)
      .partition {
        case dcl: DFVal.Dcl                           => false
        case const: DFVal.Const if !const.isAnonymous => false
        case _                                        => true
      }
    // iterator declarations within `for` loops only supported in SystemVerilog,
    // so we need to declare them at the process block level for Verilog v95/v2001
    val iteratorDcls =
      if (forInteratorDclSupport) ""
      else
        pb.members(MemberView.Flattened).view.collect { case dcl @ IteratorDcl() =>
          dcl.codeString
        }.toList.distinct.mkString(";\n").emptyOr(x => s"$x;\n")
    val body = iteratorDcls + csDFMembers(statements)
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
        if (refs.isEmpty) ""
        else s" @${refs.map(_.refCodeString).mkString("(", sensitivityListSep, ")")}"
    s"$dcl${named}$alwaysKW$senList\nbegin\n${body.hindent}\nend"
  end csProcessBlock
  val forInteratorDclSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csStepBlock(stepBlock: StepBlock): String = printer.unsupported
  def csDFForBlock(forBlock: DFLoop.DFForBlock): String =
    val body = csDFOwnerBody(forBlock)
    val rangeIR = forBlock.rangeRef.get
    val csIter = forBlock.iteratorRef.refCodeString
    val csStep = rangeIR.stepRef.refCodeString
    val csCompareOp = if (csStep.startsWith("-")) ">" else "<"
    val csCompareEq = rangeIR.op match
      case DFRange.Op.To    => "="
      case DFRange.Op.Until => ""
    val iterType = if (forInteratorDclSupport) s"${printer.csDFType(DFInt32)} " else ""
    s"for ($iterType$csIter = ${rangeIR.startRef.refCodeString}; $csIter $csCompareOp$csCompareEq ${rangeIR.endRef.refCodeString}; $csIter = $csIter + ${csStep.applyBrackets()}) begin\n${body.hindent}\nend"
  end csDFForBlock
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String =
    val body = csDFOwnerBody(whileBlock)
    s"while (${whileBlock.guardRef.refCodeString}) begin\n${body.hindent}\nend"
  end csDFWhileBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VerilogOwnerPrinter
