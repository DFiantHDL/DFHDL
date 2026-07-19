package dfhdl.compiler
package printing
import ir.*
import analysis.*
import dfhdl.internals.*
import DFVal.*
import ProcessBlock.Sensitivity
import DFConditional.DFCaseBlock.Pattern
import DFDesignBlock.InstMode
import scala.collection.immutable.ListMap

trait AbstractOwnerPrinter extends AbstractPrinter:
  // Phantom members are compiler-synthesized ports/parameters (and their by-name
  // selection and wiring artifacts) that make methods self-contained when they
  // use values from outside their own scope. The DFHDL printer hides them in the
  // method VIEW form only, so the printed def matches the user-written source
  // (its body references the captured host values by name). Once a def is dropped
  // to a regular design block, phantoms print like any other port/parameter, and
  // the backend printers always keep them.
  protected def hidePhantoms: Boolean = false
  final protected def isHiddenPhantom(member: DFMember): Boolean =
    // call-site wiring nets of a method instance are hidden through their
    // phantom port selection endpoint (input wiring is already excluded for def
    // instances, so this effectively covers phantom output captures)
    def isDefPhantomPBNS(endpoint: DFMember): Boolean = endpoint match
      case pbns: DFVal.PortByNameSelect =>
        pbns.isPhantom &&
        pbns.getDesignInst.getDesignBlock.instMode == InstMode.Def
      case _ => false
    hidePhantoms &&
    (member match
      case net: DFNet => isDefPhantomPBNS(net.lhsRef.get) || isDefPhantomPBNS(net.rhsRef.get)
      case _          => false)
  // A method declaration that references its host's values through phantoms cannot
  // print at file level: it prints locally in the host design's body, just before the
  // def's first instance (see `csDFMembers`). Only the DFHDL printer overrides this.
  protected def printMethodDclInline(design: DFDesignBlock): Boolean = false

  // ~~~ HDL method formals and actuals ~~~
  // A method's formals are its non-phantom input PORTS: an ED method's value args and a
  // static function's const args alike (const args revert to ports under the `Func`/`Op.Def`
  // call model; the `DesignParam` case below only serves DF/RT methods, which are not
  // methods).
  //
  // Phantoms are hidden from the signature: a phantom's body references print the captured
  // value's name, resolved at the host design's scope.
  final protected def methodFormals(design: DFDesignBlock): List[DFVal] =
    val returnPort = design.methodReturnPort
    design.members(MemberView.Folded).collect {
      case param: DFVal.DesignParam if !param.isPhantom => param
      case port @ DclIn() if !port.isPhantom            => port
      // an `<> OUT` argument port (a procedure's output); the function return port is excluded
      case port @ DclOut() if !port.isPhantom && !returnPort.contains(port) => port
    }
  // The call site's actual arguments, positionally matching `methodFormals`: the non-phantom
  // parameter applications, then the non-phantom input-port connections.
  final protected def defActuals(inst: DFDesignInst): List[DFVal] =
    val design = inst.getDesignBlock
    val phantomNames = getSet.designDB.phantomParamNamesOf(design)
    val paramActuals = inst.paramMap.view.collect {
      case (name, ref) if !phantomNames(name) => ref.get
    }.toList
    val instPBNS = getSet.designDB.designInstPBNS.getOrElse(
      inst,
      getSet.designDB.members.collect {
        case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => pbns
      }
    )
    val portActuals = instPBNS.view.collect {
      case pbns if pbns.isIn && !pbns.isPhantom =>
        // the positional def-instance form expects a single producer per input port; a
        // piecewise-connected input port (multiple partial nets) cannot be rendered here, so
        // we fall back to the first connection's producer.
        val DFNet.Connection(_, from: DFVal, _) = pbns.getConnectionsTo.head.runtimeChecked
        from
    }.toList
    paramActuals ++ portActuals
  end defActuals
  final def csDFOwnerBody(owner: DFOwner): String =
    csDFMembers(owner.members(MemberView.Folded))
  final def csDFMembers(members: List[DFMember]): String =
    // selecting viewable members:
    def isViewable(member: DFMember): Boolean = member match
      // excluding phantom members and their wiring
      case m if isHiddenPhantom(m) => false
      // excluding binds
      case Bind(_) => false
      // excluding design params
      case _: DFVal.DesignParam => false
      // an ident placeholder (can be anonymous)
      case Ident(_) => true
      // excluding iterator declarations
      case IteratorDcl() => false
      // an anonymous def-design inst may not be referenced later, so we
      // need to check if it has an output port that is referenced later
      case inst: DFDesignInst
          if inst.getDesignBlock.instMode == InstMode.Def && inst.isAnonymous =>
        // no output port means a Unit return that cannot be referenced,
        // so we need to print it now (an argument-less Unit call has no PBNS at all)
        getSet.designDB.designInstPBNS.getOrElse(inst, Nil).view.reverse.collectFirst {
          // no dependencies means the output is not read (referenced later),
          // so we need to print now
          case pbns if pbns.isOut => pbns.getReadDeps.isEmpty
        }.getOrElse(true)
      // a procedural (Unit-return) method call is a statement; a value-returning call
      // prints inline where it is consumed (or as a named `val` via the Named case below)
      case DFVal.Func.Call(call, _) if call.dfType =~ DFUnit => true
      // DFDesignBlock no longer participates in owner-body rendering — its
      // instantiation syntax is emitted via the DFDesignInst companion.
      case _: DFDesignBlock => false
      // named members
      case m: DFMember.Named if !m.isAnonymous => true
      // excluding late (via) connections
      case net: DFNet if net.isViaConnection => false
      // excluding nets that are inputs to a design definition
      case DFNet.Connection(toVal = PortOfMethodDesign(Modifier.IN, _)) => false
      // include the rest of statements: nets, gotos, etc.
      case _: Statement => true
      // including only conditional statements (no type) headers
      case ch: DFConditional.Header => ch.dfType =~ DFUnit
      // process blocks
      case pb: ProcessBlock => true
      // fork and local blocks
      case _: ForkBlock  => true
      case _: LocalBlock => true
      // loops
      case _: DFLoop.Block => true
      // the rest are not directly viewable
      case _ => false
    val inlinedDefDcls = scala.collection.mutable.Set.empty[DFDesignBlock]
    members.view
      .flatMap { m =>
        // a method whose declaration prints locally (see `printMethodDclInline`)
        // is emitted just before its first instance in this body. The instance member
        // always precedes the statement that consumes the def's output, so anchoring on
        // it also covers instances that themselves print inline in a later statement.
        val inlineDclOpt = m match
          case inst: DFDesignInst =>
            val design = inst.getDesignBlock
            if (printMethodDclInline(design) && inlinedDefDcls.add(design))
              // rendered through a printer carrying this call site's phantom actuals, so
              // the body names the captured values as THIS design names them
              Some(methodPrinterAt(inst).csMethodDcl(design).stripLineEnd)
            else None
          case _ => None
        val csOpt = if (isViewable(m)) Some(m.codeString) else None
        inlineDclOpt ++ csOpt
      }
      .filter(_.nonEmpty)
      .mkString("\n")
  end csDFMembers
  final def csDFDesignLateBody(inst: DFDesignInst): String =
    inst.getOwner
      .members(MemberView.Folded)
      .view
      // selecting viewable members:
      .filter {
        // late construction nets
        case net @ DFNet.Connection(toVal, fromVal, _) if net.isViaConnection =>
          // getting the nets that belong to this design
          toVal match
            case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => true
            case _                                                          =>
              fromVal match
                case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => true
                case _                                                          => false
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty).toList
      .mkString(s"${printer.csViaConnectionSep}\n")
  end csDFDesignLateBody
  def csDFDesignBlockDcl(design: DFDesignBlock): String
  def csDFDesignBlockInst(inst: DFDesignInst): String
  def csMethodDcl(design: DFDesignBlock): String
  def csMethodInst(inst: DFDesignInst): String
  def csBlockBegin: String
  def csBlockEnd: String
  def csDFIfGuard(ifBlock: DFConditional.DFIfElseBlock): String = ifBlock.guardRef.refCodeString
  def csDFIfStatement(csCond: String): String
  def csDFElseStatement: String
  def csDFElseIfStatement(csCond: String): String
  final def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => csDFIfStatement(csDFIfGuard(ifBlock))
      case _                       =>
        ifBlock.guardRef.get match
          case DFMember.Empty => csDFElseStatement
          case _              => csDFElseIfStatement(csDFIfGuard(ifBlock))
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String
  def csIfBlockEmpty: String
  def csDFCaseBlockEmpty: String
  def csDFCasePatternCatchAll: String
  def csDFCasePatternAlternativeData: String
  def csDFCasePatternStruct(pattern: Pattern.Struct): String
  def csDFCasePatternBind(pattern: Pattern.Bind): String
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String
  def csDFCasePatternNamedArg(pattern: Pattern.NamedArg): String
  def csDFCasePattern(pattern: Pattern): String = pattern match
    case Pattern.CatchAll            => csDFCasePatternCatchAll
    case Pattern.Singleton(valueRef) => valueRef.refCodeString
    case Pattern.Alternative(list)   =>
      list.map(csDFCasePattern).mkString(csDFCasePatternAlternativeData)
    case pattern: Pattern.Struct   => csDFCasePatternStruct(pattern)
    case pattern: Pattern.Bind     => csDFCasePatternBind(pattern)
    case pattern: Pattern.BindSI   => csDFCasePatternBindSI(pattern)
    case pattern: Pattern.NamedArg => csDFCasePatternNamedArg(pattern)
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String
  def csDFCaseKeyword: String
  def csDFCaseSeparator: String
  final def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String =
    val csGuard =
      caseBlock.guardRef.get match
        case DFMember.Empty => ""
        case _              => csDFCaseGuard(caseBlock.guardRef)
    s"$csDFCaseKeyword${csDFCasePattern(caseBlock.pattern)}$csGuard$csDFCaseSeparator"
  // isUnique is true when the selector is a local enum type, enabling `unique case` in
  // SystemVerilog to avoid lint warnings. Global enums are excluded because
  // their full set of entries is not guaranteed to be covered at every match site.
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean, isUnique: Boolean): String
  def csDFMatchEnd: String
  def csStepBlock(stepBlock: StepBlock): String
  def csDFForBlock(forBlock: DFLoop.DFForBlock): String
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String
  final def csDFConditionalBlock(cb: DFConditional.Block): String =
    val body = csDFOwnerBody(cb)
    val statement = cb match
      case caseBlock: DFConditional.DFCaseBlock => csDFCaseStatement(caseBlock)
      case ifBlock: DFConditional.DFIfElseBlock => csDFIfElseStatement(ifBlock)
    val end =
      if (cb.isLastCB)
        cb match
          case caseBlock: DFConditional.DFCaseBlock => ""
          case ifBlock: DFConditional.DFIfElseBlock => csDFIfEnd(ifBlock)
      else ""
    val indentBody =
      if (
        // indented body if its multiline
        body.contains("\n") ||
        // indented body if starts with an `if`
        body.startsWith("if")
      )
        s"${csBlockBegin.emptyOr(" " + _)}\n${body.hindent}${csBlockEnd.emptyOr("\n" + _)}"
      else s" $body"
    if (body.isEmpty) cb match
      case caseBlock: DFConditional.DFCaseBlock => s"$statement$csDFCaseBlockEmpty"
      case ifBlock: DFConditional.DFIfElseBlock =>
        sn"""|$statement $csIfBlockEmpty
             |$end"""
    else
      sn"""|$statement$indentBody
           |$end"""
  end csDFConditionalBlock
  final def csDFConditional(ch: DFConditional.Header): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString.applyBrackets()
        val isUnique = mh.selectorRef.get.dfType match
          case e: DFEnum => !getSet.designDB.getGlobalNamedDFTypes.contains(e)
          case _         => false
        sn"""|${csDFMatchStatement(csSelector, mh.hasWildcards, isUnique)}
             |${csChains.hindent}
             |${csDFMatchEnd}"""
      case ih: DFConditional.DFIfHeader => csChains
  def csProcessBlock(pb: ProcessBlock): String
  def csForkBlock(fb: ForkBlock): String
  def csLocalBlock(lb: LocalBlock): String
  def csDomainBlock(pb: DomainBlock): String
end AbstractOwnerPrinter

protected trait DFOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter = DFPrinter
  override protected def hidePhantoms: Boolean = true
  // HDL methods (ED methods and static functions) are excluded: they always print at the
  // top of their owning design's body (see `csDFDesignBlockDclImpl`), phantoms or not
  override protected def printMethodDclInline(design: DFDesignBlock): Boolean =
    design.instMode == InstMode.Def && !design.isHDLMethod &&
      getSet.designDB.designHasPhantoms(design)
  def csMethodDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    // if no output net, then this def has a Unit return
    var retValOpt: Option[DFVal] = None
    val outNetOpt = designMembers.view.reverse.collectFirst {
      case outNet @ DFNet.Connection(port @ DclOut(), rv: DFVal, _)
          if !port.isPhantom =>
        retValOpt = Some(rv)
        outNet
    }
    val defMembers = designMembers.filter {
      case port @ DclPort()                      => false
      case net: DFNet if outNetOpt.contains(net) => false
      case _                                     => true
    }
    val body = csDFMembers(defMembers)
    val localDcls = printer.csLocalTypeDcls(design)
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    // a procedural ED method: a `Unit` return (no return connection) under the ED domain. Its
    // input arguments render as `<> IN` (directional ports), unlike a function's `<> VAL` values.
    val isProcedure = design.domainType == DomainType.ED && retValOpt.isEmpty
    val returnPort = design.methodReturnPort
    val defArgList = designMembers.collect {
      // phantom ports materialize captured outer references, hidden from the signature.
      // A static function's formals are const-typed (`<> CONST`), which the domain
      // determines (const and non-const formals never mix in a def declaration).
      case port @ DclIn() if !port.isPhantom =>
        val csType =
          if (design.isStaticFunction) printer.csDFValConstType(port.dfType)
          else if (isProcedure) printer.csDFValPortType(port.dfType, "IN")
          else printer.csDFValType(port.dfType)
        s"${port.getName}$csType"
      // a procedure's `<> OUT` argument port (the function return port is excluded); a
      // non-blocking output prints as `<> OUT.NB`
      case port @ DclOut() if !port.isPhantom && !returnPort.contains(port) =>
        val dirCS = if (port.isNonBlockingArg) "OUT.NB" else "OUT"
        s"${port.getName}${printer.csDFValPortType(port.dfType, dirCS)}"
    }
    val methodArgsCS =
      if (defArgList.length <= 2) defArgList.mkString(", ")
      else defArgList.mkString("\n", ",\n", "\n").hindent(2)
    val designParamList = design.members(MemberView.Folded).collect {
      // phantom parameters materialize captured outer constants — hidden from the signature
      case param: DesignParam if !param.isPhantom =>
        s"${param.getName}${printer.csDFValConstType(param.dfType)}"
    }
    val designParamCS =
      if (designParamList.length == 0) ""
      else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val retDFType = retValOpt.map(_.dfType).getOrElse(DFUnit)
    // ED methods are declared with `<> EDRET` and static functions with `<> CONSTRET`; DF (and
    // RT, which currently elaborates as DF) methods with `<> DFRET`
    val retModCS = design.domainType match
      case DomainType.ED     => "EDRET"
      case DomainType.Static => "CONSTRET"
      case _                 => "DFRET"
    val retTypeCS = s": ${printer.csDFType(retDFType, typeCS = true)} <> $retModCS"
    // A method's formals are all input ports (a static function's const args included),
    // so its param list is empty; a DF/RT method keeps parameters and inputs in
    // separate lists.
    val formalsCS = s"$designParamCS($methodArgsCS)"
    val dcl =
      s"def ${design.dclName}$formalsCS$retTypeCS =\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    sn"""|${printer.csAnnotations(design.dclMeta.annotations)}
         |$dcl\n"""
  end csMethodDcl
  private def csDesignParamList(paramMap: ListMap[String, DFVal.Ref]): List[String] =
    paramMap.view.map { (name, ref) =>
      s"${name} = ${ref.refCodeString}"
    }.toList
  // drops phantom parameter applications at a method instantiation site by
  // matching the def design's phantom-tagged parameters by name
  private def nonPhantomParamMap(inst: DFDesignInst): ListMap[String, DFVal.Ref] =
    val phantomNames = getSet.designDB.phantomParamNamesOf(inst.getDesignBlock)
    if (phantomNames.isEmpty) inst.paramMap
    else inst.paramMap.filter((name, _) => !phantomNames(name))
  def csMethodInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    // `designInstPBNS` is keyed by the unified immutable insts. Elaboration-time
    // printing (e.g. test helpers that print live mutable members) holds insts
    // still carrying their pre-unification `designRef`, so a direct lookup can
    // miss; fall back to resolving the PBNSes via the current getSet, which
    // matches the member being printed.
    val instPBNS = getSet.designDB.designInstPBNS.getOrElse(
      inst,
      getSet.designDB.members.collect {
        case pbns: DFVal.PortByNameSelect if pbns.getDesignInst == inst => pbns
      }
    )
    // phantom port-by-name selects materialize captured outer references — hidden from
    // the call arguments (their body references print the captured value's name directly)
    val ports = instPBNS.view.collect {
      case pbns if pbns.isIn && !pbns.isPhantom =>
        // the positional def-instance form expects a single producer per input port;
        // a piecewise-connected input port (multiple partial nets) cannot be rendered
        // here, so we fall back to the first connection's producer.
        val DFNet.Connection(_, from: DFVal, _) = pbns.getConnectionsTo.head.runtimeChecked
        printer.csDFValRef(from, inst.getOwner)
    }.mkString(", ")
    val designParamList = csDesignParamList(nonPhantomParamMap(inst))
    val designParamCS =
      if (designParamList.length == 0) ""
      else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val dcl = s"${design.dclName}$designParamCS($ports)"
    if (inst.isAnonymous) dcl
    else s"val ${inst.getName} = $dcl"
  end csMethodInst
  def csDFDesignBlockParamInst(paramMap: ListMap[String, DFVal.Ref]): String =
    val designParamList = csDesignParamList(paramMap)
    if (designParamList.length <= 1) designParamList.mkString("(", ", ", ")")
    else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    import design.instMode
    design.foreignIPSource match
      // a foreign IP wraps a pre-existing external class; render an import of that class instead
      // of a class declaration that extends it. duplicate imports (multiple foreign IP design
      // blocks sharing the same class) are removed by the caller.
      case Some(foreign) => s"import ${foreign.clsName}\n"
      case None          => csDFDesignBlockDclImpl(design)
  end csDFDesignBlockDcl
  private def csDFDesignBlockDclImpl(design: DFDesignBlock): String =
    import design.instMode
    val localDcls = printer.csLocalTypeDcls(design)
    // HDL methods are locally scoped — their def declarations print after the design's own
    // declarations and before its statements, mirroring the HDL backends (where methods
    // live in the declarative region, after the signal/variable declarations). A method body
    // may reference any of those declarations by name.
    val edMethodDcls = printer.methodPrinters(design)
      .map((block, p) => s"${p.csDocString(block.dclMeta)}${p.csMethodDcl(block)}")
      .mkString("\n")
    val body =
      if (edMethodDcls.isEmpty) csDFOwnerBody(design)
      else
        val designMembers = design.members(MemberView.Folded)
        // a declaration prints as a `val` (ports, vars, constants, sub-design instances);
        // everything else is a statement
        def isDcl(member: DFMember): Boolean = member match
          case _: DFVal.Dcl | DclConst() | (_: DFVal.DesignParam) => true
          case inst: DFDesignInst => inst.getDesignBlock.instMode != InstMode.Def
          case _                  => false
        val (dcls, stmts) = designMembers.splitAt(designMembers.lastIndexWhere(isDcl) + 1)
        List(csDFMembers(dcls), edMethodDcls, csDFMembers(stmts))
          .filter(_.nonEmpty).mkString("\n")
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    val dsnCls = design.domainType match
      case DomainType.DF => "DFDesign"
      case DomainType.RT => "RTDesign"
      case _             =>
        design.instMode match
          case InstMode.BlackBox(source) => source match
              case InstMode.BlackBox.Source.VendorIP(_, "") =>
                "EDBlackBox.VendorIP"
              case InstMode.BlackBox.Source.VendorIP(vendor, typeName) =>
                s"dfhdl.platforms.ips.${vendor.libName}.$typeName"
              case _ => s"EDBlackBox(EDBlackBox.Source.${source})"
          case _ => "EDDesign"
    val designParams = design.members(MemberView.Folded).collect { case param: DesignParam =>
      param
    }
    val designParamList = designParams.map { param =>
      val defaultValue =
        if (design.isTop)
          if (param.appliedOrDefaultVal.hasTagOf[SyntheticDefaultTag]) ""
          else s" = ${param.appliedOrDefaultValRef.refCodeString}"
        else
          param.defaultValRef.get match
            case DFMember.Empty => ""
            case _              => s" = ${param.defaultValRef.refCodeString}"
      s"val ${param.getName}${printer.csDFValConstType(param.dfType)}$defaultValue"
    }
    // external IP blackboxes (vendor IP and foreign IP) extend a pre-existing IP class, so they
    // pass their parameters in the class extension rather than declaring/instantiating new ones
    val designIsExternalIPBlackbox = design.isExternalIPBlackbox
    val designParamDclCS =
      // for an external IP blackbox, we extend the base IP class with its parameters and declare no new parameters
      if (designIsExternalIPBlackbox) ""
      else
        if (designParamList.length == 0) ""
        else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
        else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val designParamInstCS =
      // for an external IP blackbox, we define the parameters in the class extension instead of the
      // blackbox instantiation
      if (designIsExternalIPBlackbox) csDFDesignBlockParamInst(
        ListMap.from(designParams.view.map(param =>
          param.getName -> param.defaultValRef.asInstanceOf[DFVal.Ref]
        ))
      )
      else ""
    val dcl =
      s"class ${design.dclName}$designParamDclCS extends $dsnCls$designParamInstCS"
    val dclWithBody =
      if (bodyWithDcls.isEmpty || designIsExternalIPBlackbox) dcl
      else s"$dcl:\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    sn"""|${printer.csAnnotations(design.meta.annotations)}
         |$dclWithBody\n"""
  end csDFDesignBlockDclImpl
  def csDFDesignBlockInst(inst: DFDesignInst): String =
    val design = inst.getDesignBlock
    val body = csDFDesignLateBody(inst)
    val designParamCS =
      // a vendor IP blackbox defines its parameters in the class extension, so the instantiation
      // takes no parameters; a foreign IP (rendered as an import) and a regular design both apply
      // their parameters at the instantiation
      if (design.isVendorIPBlackbox) "()"
      else csDFDesignBlockParamInst(inst.paramMap)
    val instCS =
      if (body.isEmpty) s"${design.dclName}$designParamCS"
      else s"new ${design.dclName}$designParamCS:\n${body.hindent}"
    val csVal = s"val ${inst.getName} = ${instCS}"
    if (body.isEmpty) csVal else s"$csVal\nend ${inst.getName}"
  end csDFDesignBlockInst
  def csBlockBegin: String = ""
  def csBlockEnd: String = ""
  def csDFIfStatement(csCond: String): String = s"if ($csCond)"
  def csDFElseStatement: String = "else"
  def csDFElseIfStatement(csCond: String): String = s"else if ($csCond)"
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String =
    import scala.util.boundary, boundary.break
    // check if a block is "big", meaning too many statements that should yield an "end if"
    def isBigBlock(cb: DFConditional.DFIfElseBlock): Boolean = boundary {
      var hasNet = false
      cb.members(MemberView.Folded).foreach {
        case block: DFBlock => break(true)
        case net: DFNet     =>
          if (hasNet) break(true)
          hasNet = true
        case _ =>
      }
      false
    }
    if (lastCB.getLeadingChain.exists(isBigBlock)) "end if" else ""
  end csDFIfEnd
  def csIfBlockEmpty: String = "{}"
  def csDFCaseBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "_"
  def csDFCasePatternAlternativeData: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String =
    // if there is a named arg, then we need do not print the "_" catch all patterns
    if (pattern.fieldPatterns.exists(_.isInstanceOf[Pattern.NamedArg]))
      pattern.name +
        pattern.fieldPatterns.filterNot(_ == Pattern.CatchAll).map(csDFCasePattern).mkStringBrackets
    // otherwise, printing all patterns
    else
      pattern.name + pattern.fieldPatterns.map(csDFCasePattern).mkStringBrackets
  def csDFCasePatternBind(pattern: Pattern.Bind): String =
    val bindStr = pattern.pattern match
      case Pattern.CatchAll => ""
      case _                => s" @ ${csDFCasePattern(pattern.pattern)}"
    s"${pattern.ref.get.getName}$bindStr"
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String =
    val csBinds = pattern.refs.view
      .map { r => r.get }
      .map(bindVal => s"$${${bindVal.getName}: B[${bindVal.dfType.widthIntOpt.get}]}")
    val fullTerm = pattern.parts.coalesce(csBinds).mkString
    s"""${pattern.op}"$fullTerm""""
  def csDFCasePatternNamedArg(pattern: Pattern.NamedArg): String =
    s"${pattern.name} = ${csDFCasePattern(pattern.pattern)}"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String =
    s" if ${guardRef.refCodeString}"
  def csDFCaseKeyword: String = "case "
  def csDFCaseSeparator: String = " =>"
  def csDFMatchEnd: String = "end match"
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean, isUnique: Boolean): String =
    s"$csSelector match"
  def csProcessBlock(pb: ProcessBlock): String =
    val body = csDFOwnerBody(pb)
    val named = pb.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val keyword = pb.sensitivity match
      case Sensitivity.Initial                    => "initial"
      case Sensitivity.All                        => "process(all)"
      case Sensitivity.List(refs) if refs.isEmpty => "process"
      case Sensitivity.List(refs)                 =>
        s"process${refs.map(_.refCodeString).mkStringBrackets}"
    s"${named}${keyword}:\n${body.hindent}"
  def csForkBlock(fb: ForkBlock): String =
    val body = csDFOwnerBody(fb)
    val named = fb.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val kw = fb.join match
      case ForkBlock.Join.All  => "forkJoin"
      case ForkBlock.Join.Any  => "forkJoinAny"
      case ForkBlock.Join.None => "forkJoinNone"
    s"${named}${kw}:\n${body.hindent}"
  def csLocalBlock(lb: LocalBlock): String =
    val body = csDFOwnerBody(lb)
    val named = lb.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    s"${named}locally:\n${body.hindent}"
  def csStepBlock(stepBlock: StepBlock): String =
    val body = csDFOwnerBody(stepBlock)
    val name = stepBlock.getName
    val defType =
      if (stepBlock.isRegular) ": Step"
      else if (stepBlock.isFallThrough)
        printer.csDFValType(stepBlock.getVeryLastMember.get.asInstanceOf[DFVal].dfType)
      else ": Unit"
    s"def $name$defType =\n${body.hindent}\nend $name"
  def csDFForBlock(forBlock: DFLoop.DFForBlock): String =
    val csCOMB_LOOP = if (forBlock.isCombinational) "COMB_LOOP" else ""
    val csFALL_THROUGH = if (forBlock.isFallThrough) "FALL_THROUGH" else ""
    val body =
      sn"""|${csCOMB_LOOP}
           |${csFALL_THROUGH}
           |${csDFOwnerBody(forBlock)}"""
    val named = forBlock.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val endName = forBlock.meta.nameOpt.map(n => s"end $n").getOrElse("end for")
    //format: off
    sn"""|${named}for (${forBlock.iteratorRef.refCodeString} <- ${printer.csDFRange(forBlock.rangeRef.get)})
         |${body.hindent}
         |$endName"""
    //format: on
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String =
    val csCOMB_LOOP = if (whileBlock.isCombinational) "COMB_LOOP" else ""
    val csFALL_THROUGH = if (whileBlock.isFallThrough) "FALL_THROUGH" else ""
    val body =
      sn"""|${csCOMB_LOOP}
           |${csFALL_THROUGH}
           |${csDFOwnerBody(whileBlock)}"""
    val named = whileBlock.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val endName = whileBlock.meta.nameOpt.map(n => s"end $n").getOrElse("end while")
    sn"""|${named}while (${whileBlock.guardRef.refCodeString})
         |${body.hindent}
         |$endName"""
  def csDomainBlock(domain: DomainBlock): String =
    val body = csDFOwnerBody(domain)
    val named = domain.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val endName = domain.meta.nameOpt.map(n => s"end $n").getOrElse("end new")
    val domainStr = domain.domainType match
      case DomainType.DF => "DFDomain"
      case DomainType.RT => "RTDomain"
      case DomainType.ED => "EDDomain"
      // impossible: there is no user-facing `StaticDomain` block. The static domain's only owners
      // are the global scope and a static function's def design, neither of which is a DomainBlock.
      case DomainType.Static => ???
    sn"""|${named}new $domainStr:
         |${body.hindent}
         |$endName"""
  end csDomainBlock

end DFOwnerPrinter
