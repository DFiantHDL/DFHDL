package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import DFVal.*
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFVal.Func.Op as FuncOp
import scala.collection.mutable

protected trait VerilogOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: VerilogPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "v"
  def defsName: String =
    val name = printerOptions.globalDefsFileName
    if (name.nonEmpty)
      val dotIdx = name.lastIndexOf('.')
      if (dotIdx > 0) name.substring(0, dotIdx) else name
    else s"${getSet.topName}_defs"
  def csLibrary(inSimulation: Boolean, minTimeUnitOpt: Option[TimeNumber.Unit]): String =
    val csTimeScale = minTimeUnitOpt.map { unit =>
      def unitToStr(unit: TimeNumber.Unit): String =
        unit match
          case TimeNumber.Unit.sec => "s"
          case _                   => unit.toString
      val scaleUnit = unitToStr(unit)
      val precisionUnit = unitToStr(TimeNumber(1e-3, unit).normalize.unit)
      s"`timescale 1${scaleUnit}/1${precisionUnit}"
    }.getOrElse(s"`timescale 1ns/1ps")
    sn"""|`default_nettype none
         |$csTimeScale
         |${if (printer.hasGlobalContent) s"""`include "${printer.globalFileName}"""" else ""}"""
  def moduleName(design: DFDesignBlock): String = design.dclName
  val parameterizedModuleSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 => false
      case _                  => true
  val missingParamDefaultSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  lazy val globalUsage: Map[DFDesignBlock, Set[DFVal]] =
    val globalUsage = mutable.Map.empty[DFDesignBlock, Set[DFVal]]
    getSet.designDB.membersGlobals.foreach { m =>
      if (!m.isAnonymous)
        m.originMembersNoTypeRef.foreach {
          case o: DFVal.CanBeGlobal if o.isGlobal => // do not include global members
          case o                                  =>
            val owner = o.getOwnerDesign
            globalUsage += owner -> (globalUsage.getOrElse(owner, Set()) + m)
        }
    }
    globalUsage.toMap
  end globalUsage
  def csModuleDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    val ports = designMembers.view.collect { case p @ DclPort() =>
      if (parameterizedModuleSupport) printer.csDFMember(p)
      else p.getName
    }.mkString(",\n")
    val portBlock = ports.emptyOr(v => s"""(
                                          |${ports.hindent}
                                          |)""".stripMargin)
    val localTypeDcls = printer.csLocalTypeDcls(design)
    // design parameters (non-ANSI dialects only) and the constants named by a local type
    // declaration (a vector/array width); both must precede the local type declarations
    val typeConsts = printer.typeReferencedConsts(design).toSet
    val constIntDcls =
      designMembers.view
        .flatMap {
          case p: DesignParam =>
            if (parameterizedModuleSupport) None
            else Some(p)
          case c @ DclConst() if typeConsts.contains(c) => Some(c)
          case _                                        => None
        }
        .map(x => printer.csDFMember(x) + ";")
        .mkString("\n")
    // port-related declarations that are NOT part of the value/method ordering: output ports whose
    // initial value needs an `initial` block (dialects without inline-init support), and — for
    // non-ANSI dialects that declare port directions in the module body — the port direction
    // declarations. Kept in member order.
    val portDcls =
      designMembers.view
        .flatMap {
          case p @ DclOut() if !printer.supportOutputInlineInit && p.initRefList.nonEmpty =>
            Some(printer.csDFValDclInitialBlock(p))
          case p @ DclPort() if !parameterizedModuleSupport =>
            Some(printer.csDFMember(p) + ";")
          case _ => None
        }
        .mkString("\n")
    // one constant or signal declaration; a vector signal that cannot be inline-initialized
    // additionally emits an `initial` block right after its declaration
    def csDcl(m: DFVal): List[String] = m match
      case p: DFVal.Dcl if p.isVar || !parameterizedModuleSupport =>
        // a shared variable is multi-driven by design (e.g., one clocked process per RAM port),
        // which Verilator reports on the declaration, so the suppression wraps it here
        def csDclLine =
          val cs = printer.csDFMember(p) + ";"
          if (p.modifier.isShared)
            s"""|/* verilator lint_off MULTIDRIVEN */
                |$cs
                |/* verilator lint_on MULTIDRIVEN */""".stripMargin
          else cs
        p.dfType match
          case _: DFVector if !printer.supportVectorInlineInit && p.initRefList.nonEmpty =>
            List(csDclLine, printer.csDFValDclInitialBlock(p))
          case _ => List(csDclLine)
      case c @ DclConst() => List(printer.csDFMember(c) + ";")
      case _              => Nil
    // constants, static functions, signals, and ED methods (all HDL methods are locally scoped,
    // declared in this module's declaration region) in a single stable topological order (see
    // `localDeclsOrdered`), shared with the VHDL backend. The width constants and local type
    // declarations stay ahead of them.
    val methodPrinters = printer.methodPrinters(design)
    val methodPrinterOf = methodPrinters.toMap
    def csMethodLocal(block: DFDesignBlock): String =
      val p = methodPrinterOf(block)
      sn"""|${p.csDocString(block.dclMeta)}
           |${p.csMethodDcl(block)}""".stripTrailing
    val orderedDcls = printer.joinLocalDecls(
      printer.localDeclsOrdered(design, methodPrinters.map(_._1)).flatMap {
        case LocalDecl.Const(c)        => csDcl(c).map((false, _))
        case LocalDecl.Signal(s)       => csDcl(s).map((false, _))
        case LocalDecl.StaticMethod(b) => List((true, csMethodLocal(b)))
        case LocalDecl.EDMethod(b)     => List((true, csMethodLocal(b)))
      }
    )
    val declarations =
      sn"""|$constIntDcls
           |$localTypeDcls
           |$portDcls
           |$orderedDcls"""
    val statements = csDFMembers(
      designMembers.filter {
        case _: DFVal.Dcl => false
        case DclConst()   => false
        case _            => true
      }
    )
    val designParamList = designMembers.collect { case param: DesignParam =>
      val defaultValue =
        if (design.isTopTop)
          if (param.appliedOrDefaultVal.hasTagOf[SyntheticDefaultTag]) ""
          else s" = ${param.appliedOrDefaultValRef.refCodeString}"
        else
          param.defaultValRef.get match
            case DFMember.Empty =>
              // missing default values are supported
              if (missingParamDefaultSupport) ""
              // missing default values are not supported, so we fetch a valid constant data
              // (different instances may have different constant data, but for default,
              // a single module description can have any valid data, just to satisfy the standard)
              else
                s" = ${printer.csDesignParamDefault(param)}"
            case _ => s" = ${printer.csDesignParamDefault(param)}"
      val csType = printer.csDFType(param.dfType).emptyOr(_ + " ")
      val csTypeNoLogic = if (printer.supportLogicType) csType else csType.replace("logic ", "")
      s"parameter ${csTypeNoLogic}${param.getName}$defaultValue"
    }
    val designParamCS =
      if (designParamList.length == 0 || !parameterizedModuleSupport) ""
      else if (designParamList.length == 1) designParamList.mkString("#(", ", ", ")")
      else "#(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val includeModuleDefs =
      if (printer.allowTypeDef || !printer.hasGlobalContent) ""
      else s"""`include "${printer.globalFileName}""""
    // include parameter definitions only when parameters are used in the design
    val paramDefines =
      if (printer.supportGlobalParameters) ""
      else globalUsage.getOrElse(design, Set()).view.map(m =>
        s"`${m.getName}_def"
      ).toList.sorted.mkString("\n")
    sn"""|module ${moduleName(design)}$designParamCS$portBlock;
         |  `include "dfhdl_defs.${printer.verilogFileHeaderSuffix}"
         |${includeModuleDefs.hindent}
         |${paramDefines.hindent}
         |${declarations.hindent}
         |${statements.hindent}
         |endmodule"""
  end csModuleDcl
  lazy val minTimeUnitDesignMap = getSet.designDB.designMemberList.view.flatMap { (dsn, members) =>
    val minTimePSOpt = members.view.collect {
      case DFVal.Const(dfType = DFTime, data = time @ TimeNumber(_, _)) =>
        time.to_ps.value
    }.minOption
    minTimePSOpt.map(ps => dsn -> TimeNumber(ps, TimeNumber.Unit.ps).normalize.unit)
  }.toMap
  lazy val minTimeUnitGlobalOpt =
    minTimeUnitDesignMap.values.view.map(unit => TimeNumber(1, unit).to_ps.value).minOption.map(
      ps =>
        TimeNumber(ps, TimeNumber.Unit.ps).normalize.unit
    )
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    // once there is a design with a set time unit, all designs must have a set time unit,
    // so we can use the global time unit if the design does not have a set time unit
    val minTimeUnitOpt = minTimeUnitDesignMap.get(design).orElse(minTimeUnitGlobalOpt)
    s"""${csLibrary(design.inSimulation, minTimeUnitOpt)}
       |
       |${csModuleDcl(design)}
       |""".stripMargin
  def csDFDesignBlockInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    val body = csDFDesignLateBody(inst)
    val designParamList = inst.paramMap.view.map { (name, ref) =>
      s".${name} (${ref.refCodeString})"
    }.toList
    val designParamCS =
      if (designParamList.isEmpty || design.isVendorIPBlackbox) ""
      else " #(" + designParamList.mkString("\n", ",\n", "\n").hindent(1) + ")"
    val instCS = s"${moduleName(design)}$designParamCS ${inst.getName}"
    if (body.isEmpty) s"$instCS;" else s"$instCS(\n${body.hindent}\n);"
  // v95/v2001 require at least one function input; when the printed input list is empty
  // (no explicit args — phantoms are hidden), a dummy input is declared and call sites
  // pass a literal `0`
  val dummyLessFunctionSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  // `ref` task/function arguments (needed for a live `<> OUT.NB` output) exist only in
  // SystemVerilog; plain Verilog (v95/v2001) has no way to express a live output argument
  val refArgSupport: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  // ED methods (HDL functions) print inside their owning module's declaration region
  def csMethodDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    // the return value: the (single, full) connection to the output port
    var retValOpt: Option[DFVal] = None
    val outNetOpt = designMembers.view.reverse.collectFirst {
      case outNet @ DFNet.Connection(DclOut(), rv: DFVal, _) =>
        retValOpt = Some(rv)
        outNet
    }
    val funcName = design.dclName
    val ansiHeader = printer.dialect match
      case VerilogDialect.v95 => false
      case _                  => true
    // HDL methods are static by default — `automatic` gives the expected
    // fresh-per-call semantics (unavailable in v95, where functions cannot wait anyway).
    //
    // NOTE the naming collision, before someone "fixes" it: SystemVerilog's `static` is a
    // variable LIFETIME and is the opposite of `automatic`, and has nothing to do with DFHDL's
    // static DOMAIN. A static function therefore emits as an `automatic` function.
    val automatic = if (ansiHeader) "automatic " else ""
    def csFuncType(dfType: DFType): String =
      val csType = printer.csDFType(dfType).emptyOr(_ + " ")
      if (printer.supportLogicType) csType else csType.replace("logic ", "")
    val retTypeCS = retValOpt.map(rv => csFuncType(rv.dfType)).getOrElse("")
    // a method's formals: design parameters and/or input ports, in one list (see
    // `methodFormals`). Verilog has no method generics, so a static function's parameters
    // print as ordinary input formals.
    val inputs = methodFormals(design)
    // a copy-out `<> OUT` argument prints as a task `output` (copied back on return); a
    // non-blocking `<> OUT.NB` needs a LIVE output, which a Verilog task `output` cannot give
    // (its copy-out happens at return, before a scheduled non-blocking update), so it lowers to
    // a SystemVerilog `ref` argument. Plain Verilog has no `ref`, so it is unsupported there.
    def csInput(p: DFVal): String =
      val dirCS =
        if (p.isNonBlockingArg)
          if (refArgSupport) "ref"
          else
            throw new IllegalArgumentException(
              s"A non-blocking output argument (`<> OUT.NB`) of `${design.dclName}` requires a SystemVerilog `ref` argument, which plain Verilog (v95/v2001) does not support. Use a SystemVerilog dialect, or change the argument to a copy-out `<> OUT`."
            )
        else if (p.isPortOut) "output"
        else "input"
      s"$dirCS ${csFuncType(p.dfType)}${p.getName}"
    // a procedural method (Unit return — no return output port) prints as a task
    val isProcedural = retValOpt.isEmpty
    val method = if (isProcedural) "task" else "function"
    // the v95/v2001 minimum-one-input rule applies to functions only; tasks may have no
    // arguments in all dialects
    val needsDummy = !isProcedural && inputs.isEmpty && !dummyLessFunctionSupport
    val header =
      if (ansiHeader)
        val inputList =
          if (needsDummy) "input __dummy__"
          else inputs.map(csInput).mkString(", ")
        // a parameterless task is declared without parentheses (portable across dialects)
        if (isProcedural && inputs.isEmpty) s"task automatic $funcName;"
        else s"$method $automatic$retTypeCS$funcName($inputList);"
      else
        val inputDcls =
          (if (needsDummy) List("input __dummy__;") else inputs.map(csInput(_) + ";"))
            .mkString("\n")
        val headerLine = s"$method $retTypeCS$funcName;"
        if (inputDcls.isEmpty) headerLine
        else s"$headerLine\n${inputDcls.hindent}"
    val localDcls = designMembers.view.flatMap {
      // phantom design parameters (captured outer constants) print nowhere — their body
      // references resolve to the captured constant's name at module scope
      case _: DesignParam              => None
      case dcl: DFVal.Dcl if dcl.isVar => Some(printer.csDFMember(dcl) + ";")
      case c @ DclConst()              => Some(printer.csDFMember(c) + ";")
      case _                           => None
    }.mkString("\n")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl                          => false
      case DclConst()                            => false
      case net: DFNet if outNetOpt.contains(net) => false
      // the return value ident placeholder is rendered by the return assignment below
      case m: DFVal if retValOpt.contains(m) => false
      case _                                 => true
    })
    val retAssign =
      retValOpt.map(rv => s"$funcName = ${printer.csDFValRef(rv, design)};").getOrElse("")
    val body =
      sn"""|$statements
           |$retAssign"""
    sn"""|$header
         |${localDcls.hindent}
         |begin
         |${body.hindent}
         |end
         |end$method"""
  end csMethodDcl
  def csMethodInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    val instPBNS = getSet.designDB.designInstPBNS.getOrElse(
      inst,
      getSet.designDB.members.collect {
        case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => pbns
      }
    )
    // a procedural method call (no return output port) is a statement
    val isProcedural = !instPBNS.exists(_.isOut)
    // the actuals positionally match `methodFormals`: a static function's are its applied design
    // parameters, an ED method's are its input-port connections
    val args = defActuals(inst).map(printer.csDFValRef(_, inst.getOwner)).mkString(", ")
    if (isProcedural)
      // a parameterless task call has no parentheses
      if (args.isEmpty) s"${moduleName(design)};"
      else s"${moduleName(design)}($args);"
    else
      val argList = if (args.isEmpty && !dummyLessFunctionSupport) "0" else args
      s"${moduleName(design)}($argList)"
  end csMethodInst
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
  def csDFCasePatternNamedArg(pattern: Pattern.NamedArg): String = printer.unsupported
  // case patterns print bubble digits as the wildcard `?`, while value positions print the
  // don't-care digit `x`. An anonymous singleton const prints exactly as its `refCodeString`
  // form would (`csConstData`), only with the pattern flag raised.
  override def csDFCasePattern(pattern: Pattern): String = pattern match
    case Pattern.Singleton(DFRef(const: DFVal.Const)) if const.isAnonymous =>
      printer.csConstData(const.dfType, const.data, inPattern = true)
    case _ => super.csDFCasePattern(pattern)
  def csDFCaseKeyword: String = ""
  def csDFCaseSeparator: String = ":"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String = printer.unsupported
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean, isUnique: Boolean): String =
    val insideSupport = printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => false
      case _                                         => true
    val uniqueSupport = printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => false
      case _                                         => true
    val uniquePrefix =
      if (isUnique && uniqueSupport) "unique " else ""
    val keyWord = if (wildcardSupport && !insideSupport) "casez" else "case"
    val insideStr = if (wildcardSupport && insideSupport) " inside" else ""
    s"$uniquePrefix$keyWord ($csSelector)$insideStr"
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
    val alwaysKW = pb.sensitivity match
      case Sensitivity.Initial => "initial"
      case _                   =>
        printer.dialect match
          case VerilogDialect.v2001 | VerilogDialect.v95 => "always"
          case _                                         =>
            pb.sensitivity.runtimeChecked match
              case Sensitivity.All        => "always_comb"
              case Sensitivity.List(refs) =>
                refs match
                  case DFRef(DFVal.Func(op = FuncOp.rising | FuncOp.falling)) :: Nil =>
                    "always_ff"
                  case DFRef(DFVal.Func(op = FuncOp.rising | FuncOp.falling)) ::
                      DFRef(DFVal.Func(op = FuncOp.rising | FuncOp.falling)) :: Nil =>
                    "always_ff"
                  case _ => "always"
    val senList = pb.sensitivity match
      case Sensitivity.Initial    => ""
      case Sensitivity.All        => if (alwaysKW == "always") " @(*)" else ""
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) ""
        else s" @${refs.map(_.refCodeString).mkString("(", sensitivityListSep, ")")}"
    s"$dcl${named}$alwaysKW$senList\nbegin\n${body.hindent}\nend"
  end csProcessBlock
  def csForkBlock(fb: ForkBlock): String =
    // `join_any` / `join_none` exist only in SystemVerilog (sv2005+). Old Verilog (v95/v2001) has
    // only `fork ... join` (wait-all); the other modes are never lowered for it (see DropForkJoins)
    // and must be rejected here rather than emitted as invalid code.
    val joinAnyNoneSupported = printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
    if (fb.join != ForkBlock.Join.All && !joinAnyNoneSupported) printer.unsupported
    else
      val body = csDFMembers(fb.members(MemberView.Folded))
      val label = fb.meta.nameOpt.map(n => s" : $n").getOrElse("")
      val joinKW = fb.join match
        case ForkBlock.Join.All  => "join"
        case ForkBlock.Join.Any  => "join_any"
        case ForkBlock.Join.None => "join_none"
      s"fork$label\n${body.hindent}\n$joinKW"
  end csForkBlock
  def csLocalBlock(lb: LocalBlock): String =
    val (statements, dcls) = lb
      .members(MemberView.Folded)
      .partition {
        case dcl: DFVal.Dcl                           => false
        case const: DFVal.Const if !const.isAnonymous => false
        case _                                        => true
      }
    val label = lb.meta.nameOpt.map(n => s" : $n").getOrElse("")
    val dcl =
      if (dcls.isEmpty) ""
      else s"${csDFMembers(dcls)}\n"
    val body = dcl + csDFMembers(statements)
    s"begin$label\n${body.hindent}\nend"
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
    sn"""|for ($iterType$csIter = ${rangeIR.startRef.refCodeString}; $csIter $csCompareOp$csCompareEq ${rangeIR.endRef.refCodeString}; $csIter = $csIter + ${csStep.applyBrackets()}) begin
         |${body.hindent}
         |end"""
  end csDFForBlock
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String =
    val body = csDFOwnerBody(whileBlock)
    sn"""|while (${whileBlock.guardRef.refCodeString}) begin
         |${body.hindent}
         |end"""
  end csDFWhileBlock
  def csDomainBlock(pb: DomainBlock): String = printer.unsupported
end VerilogOwnerPrinter
