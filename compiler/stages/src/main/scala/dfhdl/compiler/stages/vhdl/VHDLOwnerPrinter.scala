package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern
import scala.collection.mutable
import scala.collection.immutable.ListSet
protected trait VHDLOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: VHDLPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "vhdl"
  def packageName: String =
    val name = printerOptions.globalDefsFileName
    if (name.nonEmpty)
      val dotIdx = name.lastIndexOf('.')
      if (dotIdx > 0) name.substring(0, dotIdx) else name
    else s"${getSet.topName}_pkg"
  def csLibrary(inSimulation: Boolean, usesMathReal: Boolean): String =
    val default =
      sn"""|library ieee;
           |use ieee.std_logic_1164.all;
           |use ieee.numeric_std.all;
           |${if (usesMathReal) "use ieee.math_real.all;" else ""}
           |use work.dfhdl_pkg.all;
           |${if (printer.hasGlobalContent) s"use work.$packageName.all;" else ""}"""
    if (useStdSimLibrary && inSimulation)
      s"""$default
         |
         |library std;
         |use std.env.all;""".stripMargin
    else default
  def entityName(design: DFDesignBlock): String = design.dclName
  def csEntityDcl(design: DFDesignBlock, asComponent: Boolean = false): String =
    val designMembers = design.members(MemberView.Folded)
    val ports = designMembers.view
      .collect { case p @ DclPort() =>
        printer.csDFMember(p)
      }
      .mkString(";\n")
    val designParamList = designMembers.collect { case param: DesignParam =>
      val defaultValue =
        if (design.isTop)
          if (param.appliedOrDefaultVal.hasTagOf[SyntheticDefaultTag]) ""
          else s" := ${param.appliedOrDefaultValRef.refCodeString}"
        else
          param.defaultValRef.get match
            case DFMember.Empty => ""
            case _              => s" := ${param.defaultValRef.refCodeString}"
      s"${param.getName} : ${printer.csDFType(param.dfType)}$defaultValue"
    }
    val genericBlock =
      if (designParamList.length == 0 || design.isVendorIPBlackbox) ""
      else "generic (" + designParamList.mkString("\n", ";\n", "\n").hindent(1) + ");"
    val portBlock = ports.emptyOr(v => s"""|port (
                                           |${ports.hindent}
                                           |);""".stripMargin)
    val entityOrComponent = if (asComponent) "component" else "entity"
    val endComponent = if (asComponent) " component" else ""
    sn"""|$entityOrComponent ${entityName(design)} is
         |$genericBlock
         |$portBlock
         |end$endComponent ${entityName(design)};"""
  end csEntityDcl
  def archName(design: DFDesignBlock): String = s"${design.dclName}_arch"
  def csArchitectureDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    // collecting all the vhdl named types that are used in conversion to/from bits
    val vhdlNamedConvDFTypes = design.members(MemberView.Flattened).view.flatMap {
      case alias: DFVal.Alias.AsIs =>
        val pf: PartialFunction[DFType, (DFVector | NamedDFType)] = {
          case dt: (DFVector | NamedDFType) => dt
        }
        (alias.dfType, alias.relValRef.get.dfType) match
          case (DFBits(_), fromDFType: (NamedDFType | ComposedDFType)) =>
            fromDFType.decompose(pf)
          case (toDFType: (NamedDFType | ComposedDFType), DFBits(_)) =>
            toDFType.decompose(pf)
          case _ => None
      case _ => None
    }.toSet
    // the vectors requiring conversion to/from bits
    val vectorsConvUsed = vhdlNamedConvDFTypes.collect { case dfType: DFVector =>
      printer.getVecDepthAndCellTypeName(dfType)._1
    }
    // In VHDL the vectors need to be named, and put in dependency order of other named types.
    // So first we prepare the vector type declarations in a mutable map and later we remove
    // entries that were already placed in the final type printing.
    val vectorTypeDcls =
      mutable.Map.from(printer.getLocalVectorTypes(design).view.map {
        case (tpName, (vecType, depth)) =>
          val dclScope =
            if (vectorsConvUsed.contains(tpName)) DclScope.ArchBody else DclScope.TypeOnly
          tpName -> printer.csDFVectorDclsLocal(dclScope)(tpName, vecType, depth)
      })
    val globalNamedDFTypes = getSet.designDB.getGlobalNamedDFTypes
    // collect the local named types, including vectors
    val namedDFTypes = ListSet.from(getSet.designDB.designMemberTable(design).view.collect {
      case localVar @ DclVar()     => localVar.dfType
      case localConst @ DclConst() => localConst.dfType
    }.flatMap(_.decompose[DFVector | NamedDFType] {
      case dt: DFVector                                        => dt
      case dt: NamedDFType if !globalNamedDFTypes.contains(dt) => dt
    }))
    // declarations of the types and relevant functions
    val namedTypeConvFuncsDcl = namedDFTypes.view
      .flatMap {
        // vector types can have different dimensions, but we only need the declaration once
        case dfType: DFVector =>
          val tpName = printer.getVecDepthAndCellTypeName(dfType)._1
          vectorTypeDcls.get(tpName) match
            case Some(desc) =>
              vectorTypeDcls -= tpName
              Some(desc)
            case None => None
        case dfType: NamedDFType =>
          if (vhdlNamedConvDFTypes.contains(dfType))
            List(
              printer.csNamedDFTypeDcl(dfType, global = false),
              printer.csNamedDFTypeConvFuncsBody(dfType)
            )
          else Some(printer.csNamedDFTypeDcl(dfType, global = false))
      }
      .mkString("\n")

    val constIntDcls =
      designMembers.view
        .flatMap {
          case _: DesignParam => None
          case c @ DclConst() =>
            c.dfType match
              case DFInt32 => Some(c)
              case _       => None
          case _ => None
        }
        .map(printer.csDFMember)
        .mkString("\n")
    val dfValDcls =
      designMembers.view
        .flatMap {
          case _ @IteratorDcl()        => None
          case p: DFVal.Dcl if p.isVar => Some(p)
          case _: DesignParam          => None
          case c @ DclConst()          =>
            c.dfType match
              case DFInt32 => None
              case _       => Some(c)
          case _ => None
        }
        .map(printer.csDFMember)
        .mkString("\n")
    // Foreign IPs supply their own HDL wrapper (compiled into the `work` library at simulate
    // time), so they are instanced directly via `entity work.<name>(rtl)` and need no component
    // declaration here. Other blackboxes (e.g. vendor IPs) still require a component declaration.
    val components = designMembers.view.collect {
      case inst: DFDesignInst
          if inst.getDesignBlock.isBlackBox && !inst.getDesignBlock.isForeignIPBlackbox =>
        inst.getDesignBlock
    }.map(bb => printerForDesign(bb).csEntityDcl(bb, asComponent = true)).mkString("\n")
    // ED methods (HDL functions) are locally scoped — declared in this design's
    // architecture declarative part
    val edMethodDcls = printer.edMethodPrinters(design)
      .map((block, p) => s"${p.csDocString(block.dclMeta)}${p.csDFDesignDefDcl(block)}")
      .mkString("\n\n")
    val declarations =
      sn"""|$constIntDcls
           |$namedTypeConvFuncsDcl
           |$dfValDcls
           |$edMethodDcls
           |$components"""
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl => false
      case DclConst()   => false
      case _            => true
    })
    sn"""|architecture ${archName(design)} of ${design.dclName} is
         |${declarations.hindent}
         |begin
         |${statements.hindent}
         |end ${archName(design)};"""
  end csArchitectureDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    val usesMathReal = design.members(MemberView.Folded).exists {
      case v: DFVal =>
        v.dfType.decompose { case dt @ DFDouble => dt }.nonEmpty
      case _ => false
    }
    s"""${csLibrary(design.inSimulation, usesMathReal)}
       |
       |${csEntityDcl(design)}
       |
       |${csArchitectureDcl(design)}
       |""".stripMargin
  end csDFDesignBlockDcl
  def csDFDesignBlockInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    val body = csDFDesignLateBody(inst)
    val designParamList = inst.paramMap.view.map { (name, ref) =>
      s"${name} => ${ref.refCodeString}"
    }.toList
    val designParamCS =
      if (designParamList.isEmpty || design.isVendorIPBlackbox) ""
      else " generic map (" + designParamList.mkString("\n", ",\n", "\n").hindent(1) + ")"
    val header =
      // Foreign IPs are compiled into the `work` library from their bundled wrapper, so they are
      // instanced directly like regular designs. Their wrapper always uses the `rtl` architecture.
      if (design.isForeignIPBlackbox) s"entity work.${entityName(design)}(rtl)"
      // other blackboxes (e.g. vendor IPs) use a component declaration, so the header is just the
      // entity name
      else if (design.isBlackBox) entityName(design)
      else s"entity work.${entityName(design)}(${archName(design)})"
    val instCS = s"${inst.getName} : $header${designParamCS}"
    if (body.isEmpty) s"$instCS;" else s"$instCS port map (\n${body.hindent}\n);"
  end csDFDesignBlockInst
  // ED methods (HDL functions) print inside their owning design's architecture
  // declarative part
  def csDFDesignDefDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    // the return value: the (single, full) connection to the output port
    var retValOpt: Option[DFVal] = None
    val outNetOpt = designMembers.view.reverse.collectFirst {
      case outNet @ DFNet.Connection(DclOut(), rv: DFVal, _) =>
        retValOpt = Some(rv)
        outNet
    }
    val funcName = design.dclName
    // a function reading anything beyond its parameters (phantom-captured outer
    // references) must be declared impure
    val hasPhantoms = designMembers.exists {
      case p @ DclIn() => p.isPhantom
      case _           => false
    }
    val impure = if (hasPhantoms) "impure " else ""
    // a procedural method (Unit return — no return output port) prints as a procedure
    val isProcedural = retValOpt.isEmpty
    // VHDL function return takes a type MARK (no constraint)
    val retTypeCS = retValOpt.map(rv => printer.csDFType(rv.dfType).takeWhile(_ != '('))
    val params = designMembers.collect {
      // phantom ports materialize captured outer references — hidden from the signature
      case p @ DclIn() if !p.isPhantom =>
        s"${p.getName} : ${printer.csDFType(p.dfType)}"
    }.mkString("; ")
    // parameterless VHDL subprograms are declared (and called) without parentheses
    val paramsCS = params.emptyOr(p => s"($p)")
    val localDcls = designMembers.view.flatMap {
      // phantom design parameters (captured outer constants) print nowhere — their body
      // references resolve to the captured constant's name at architecture scope
      case _: DesignParam              => None
      case dcl: DFVal.Dcl if dcl.isVar => Some(printer.csDFMember(dcl))
      case c @ DclConst()              => Some(printer.csDFMember(c))
      case _                           => None
    }.mkString("\n")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl                          => false
      case DclConst()                            => false
      case net: DFNet if outNetOpt.contains(net) => false
      // the return value ident placeholder is rendered by the return statement below
      case m: DFVal if retValOpt.contains(m) => false
      case _                                 => true
    })
    val retStatement =
      retValOpt.map(rv => s"return ${printer.csDFValRef(rv, design)};").getOrElse("")
    val body =
      sn"""|$statements
           |$retStatement"""
    // procedures take no purity keyword; a phantom-reading procedure is simply legal VHDL
    val headerCS =
      if (isProcedural) s"procedure $funcName$paramsCS is"
      else s"${impure}function $funcName$paramsCS return ${retTypeCS.get} is"
    val endCS = if (isProcedural) "end procedure;" else "end function;"
    sn"""|$headerCS
         |${localDcls.hindent}
         |begin
         |${body.hindent}
         |$endCS"""
  end csDFDesignDefDcl
  def csDFDesignDefInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    val instPBNS = getSet.designDB.designInstPBNS.getOrElse(
      inst,
      getSet.designDB.members.collect {
        case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => pbns
      }
    )
    val args = instPBNS.view.collect {
      case pbns if pbns.isIn && !pbns.isPhantom =>
        val DFNet.Connection(_, from: DFVal, _) = pbns.getConnectionsTo.head.runtimeChecked
        printer.csDFValRef(from, inst.getOwner)
    }.mkString(", ")
    // a procedural method call (no return output port) is a statement;
    // parameterless VHDL subprogram calls have no parentheses
    val isProcedural = !instPBNS.exists(_.isOut)
    val callCS =
      if (args.isEmpty) design.dclName
      else s"${design.dclName}($args)"
    if (isProcedural) s"$callCS;" else callCS
  end csDFDesignDefInst
  def csBlockBegin: String = ""
  def csBlockEnd: String = ""
  override def csDFIfGuard(ifBlock: DFConditional.DFIfElseBlock): String =
    printer.csFixedCond(ifBlock.guardRef.asInstanceOf[DFRef.TwoWay[DFVal, ?]])
  def csDFIfStatement(csCond: String): String = s"if $csCond then"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"elsif $csCond then"
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String = "end if;"
  def csIfBlockEmpty: String = "null;"
  def csDFCaseBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "others"
  def csDFCasePatternAlternativeData: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String = printer.unsupported
  def csDFCasePatternBind(pattern: Pattern.Bind): String = printer.unsupported
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String = printer.unsupported
  def csDFCasePatternNamedArg(pattern: Pattern.NamedArg): String = printer.unsupported
  def csDFCaseKeyword: String = "when "
  def csDFCaseSeparator: String = " =>"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean, isUnique: Boolean): String =
    s"case $csSelector is"
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
    val csDcls = csDFMembers(dcls)
    val named = pb.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All        => " (all)"
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" ${refs.map(_.refCodeString).mkStringBrackets}"
      // initial blocks never reach VHDL printing (SplitInitialBlocks lowers them beforehand)
      case Sensitivity.Initial => printer.unsupported
    sn"""|${named}process$senList
         |${csDcls.hindent}
         |begin
         |${body.hindent}
         |end process;"""
  end csProcessBlock
  // fork-join and local blocks are lowered away (DropForkJoinsED / DropLocalBlocksED) before
  // VHDL printing; these are only safety nets.
  def csForkBlock(fb: ForkBlock): String = printer.unsupported
  def csLocalBlock(lb: LocalBlock): String = printer.unsupported
  def csStepBlock(stepBlock: StepBlock): String = printer.unsupported
  def csDFForBlock(forBlock: DFLoop.DFForBlock): String =
    val body = csDFOwnerBody(forBlock)
    val named = forBlock.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val rangeIR = forBlock.rangeRef.get
    val csRange =
      rangeIR.stepRef.refCodeString match
        case "1" =>
          val csOpExtra = rangeIR.op match
            case DFRange.Op.To    => ""
            case DFRange.Op.Until => "-1"
          s"${rangeIR.startRef.refCodeString} to ${rangeIR.endRef.refCodeString}$csOpExtra"
        case "-1" =>
          val csOpExtra = rangeIR.op match
            case DFRange.Op.To    => ""
            case DFRange.Op.Until => "+1"
          s"${rangeIR.startRef.refCodeString} downto ${rangeIR.endRef.refCodeString}$csOpExtra"
        case _ => printer.unsupported
    sn"""|${named}for ${forBlock.iteratorRef.refCodeString} in $csRange loop
         |${body.hindent}
         |end loop;"""
  end csDFForBlock
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String =
    val body = csDFOwnerBody(whileBlock)
    val guard = printer.csFixedCond(whileBlock.guardRef)
    sn"""|while $guard loop
         |${body.hindent}
         |end loop;"""
  end csDFWhileBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VHDLOwnerPrinter
