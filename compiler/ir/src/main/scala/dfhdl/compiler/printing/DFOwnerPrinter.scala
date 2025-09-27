package dfhdl.compiler
package printing
import ir.*
import analysis.*
import dfhdl.internals.*
import DFVal.*
import ProcessBlock.Sensitivity
import DFConditional.DFCaseBlock.Pattern
import DFDesignBlock.InstMode

trait AbstractOwnerPrinter extends AbstractPrinter:
  final def csDFOwnerBody(owner: DFOwner): String =
    csDFMembers(owner.members(MemberView.Folded))
  final def csDFMembers(members: List[DFMember]): String =
    members.view
      // selecting viewable members:
      .filter {
        // excluding binds
        case Bind(_) => false
        // excluding design params
        case _: DFVal.DesignParam => false
        // an ident placeholder (can be anonymous)
        case Ident(_) => true
        // excluding iterator declarations
        case IteratorDcl() => false
        // a def design that is anonymous may not be referenced later,
        // so we need to check if it has an output port that is referenced later
        case design: DFDesignBlock if design.instMode == InstMode.Def && design.isAnonymous =>
          design.members(MemberView.Folded).view.reverse.collectFirst { case port @ DclOut() =>
            // no dependencies means the output is not read (referenced later),
            // so we need to print now
            port.getReadDeps.isEmpty
          }
            // no output port means a Unit return that cannot be referenced,
            // so we need to print it now
            .getOrElse(true)
        // named members
        case m: DFMember.Named if !m.isAnonymous => true
        // excluding late (via) connections
        case net: DFNet if net.isViaConnection => false
        // excluding nets that are inputs to a design definition
        case DFNet.Connection(toVal = PortOfDesignDef(Modifier.IN, _)) => false
        // include the rest of statements: nets, gotos, etc.
        case _: Statement => true
        // including only conditional statements (no type) headers
        case ch: DFConditional.Header => ch.dfType =~ DFUnit
        // process blocks
        case pb: ProcessBlock => true
        // loops
        case _: DFLoop.Block => true
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  end csDFMembers
  final def csDFDesignLateBody(design: DFDesignBlock): String =
    design.getOwner
      .members(MemberView.Folded)
      .view
      // selecting viewable members:
      .filter {
        // late construction nets
        case net @ DFNet.Connection(toVal, fromVal, _) if net.isViaConnection =>
          // getting the nets that belong to this design
          toVal.isInsideOwner(design) || fromVal.isInsideOwner(design)
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty).toList
      .mkString(s"${printer.csViaConnectionSep}\n")
  end csDFDesignLateBody
  def csDFDesignBlockDcl(design: DFDesignBlock): String
  def csDFDesignBlockInst(design: DFDesignBlock): String
  def csDFDesignDefDcl(design: DFDesignBlock): String
  def csDFDesignDefInst(design: DFDesignBlock): String
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
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean): String
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
        sn"""|${csDFMatchStatement(csSelector, mh.hasWildcards)}
             |${csChains.hindent}
             |${csDFMatchEnd}"""
      case ih: DFConditional.DFIfHeader => csChains
  def csProcessBlock(pb: ProcessBlock): String
  def csDomainBlock(pb: DomainBlock): String
end AbstractOwnerPrinter

protected trait DFOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter = DFPrinter
  def csDFDesignDefDcl(design: DFDesignBlock): String =
    val designMembers = design.members(MemberView.Folded)
    // if no output net, then this def has a Unit return
    var retValOpt: Option[DFVal] = None
    val outNetOpt = designMembers.view.reverse.collectFirst {
      case outNet @ DFNet.Connection(DclOut(), rv: DFVal, _) =>
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
    val defArgList = designMembers.collect { case port @ DclIn() =>
      s"${port.getName}${printer.csDFValType(port.dfType)}"
    }
    val defArgsCS =
      if (defArgList.length <= 2) defArgList.mkString(", ")
      else defArgList.mkString("\n", ",\n", "\n").hindent(2)
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      s"${param.getName}${printer.csDFValConstType(param.dfType)}"
    }
    val designParamCS =
      if (designParamList.length == 0) ""
      else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val retDFType = retValOpt.map(_.dfType).getOrElse(DFUnit)
    val retTypeCS = s": ${printer.csDFType(retDFType, typeCS = true)} <> DFRET"
    val dcl =
      s"def ${design.dclName}$designParamCS($defArgsCS)$retTypeCS =\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    s"${printer.csAnnotations(design.dclMeta.annotations)}$dcl\n"
  end csDFDesignDefDcl
  def csDFDesignDefInst(design: DFDesignBlock): String =
    val ports = design.members(MemberView.Folded).view.collect { case port @ DclIn() =>
      val DFNet.Connection(_, from: DFVal, _) = port.getConnectionTo.get: @unchecked
      printer.csDFValRef(from, design.getOwner)
    }.mkString(", ")
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      s"${param.getName} = ${param.dfValRef.refCodeString}"
    }
    val designParamCS =
      if (designParamList.length == 0) ""
      else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val dcl = s"${design.dclName}$designParamCS($ports)"
    if (design.isAnonymous) dcl
    else s"val ${design.getName} = $dcl"
  end csDFDesignDefInst
  def csDFDesignBlockParamInst(design: DFDesignBlock): String =
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      s"${param.getName} = ${param.dfValRef.refCodeString}"
    }
    if (designParamList.length <= 1) designParamList.mkString("(", ", ", ")")
    else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    import design.instMode
    val localDcls = printer.csLocalTypeDcls(design)
    val body = csDFOwnerBody(design)
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    val dsnCls = design.domainType match
      case DomainType.DF     => "DFDesign"
      case rt: DomainType.RT =>
        val cfgStr = rt.cfg match
          case RTDomainCfg.Derived => ""
          case _                   => s"(${printer.csRTDomainCfg(rt.cfg)})"
        s"""RTDesign$cfgStr""".stripMargin
      case _ =>
        design.instMode match
          case InstMode.BlackBox(source) => source match
              case InstMode.BlackBox.Source.Qsys("") =>
                "EDBlackBox.QsysIP"
              case InstMode.BlackBox.Source.Qsys(typeName) =>
                s"dfhdl.platforms.ips.alteraintel.$typeName"
              case _ => s"EDBlackBox(EDBlackBox.Source.${source})"
          case _ => "EDDesign"
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      val defaultValue =
        if (design.isTop) s" = ${param.dfValRef.refCodeString}"
        else
          param.defaultRef.get match
            case DFMember.Empty => ""
            case _              => s" = ${param.defaultRef.refCodeString}"
      s"val ${param.getName}${printer.csDFValConstType(param.dfType)}$defaultValue"
    }
    val designIsQsysIPBlackbox = design.isQsysIPBlackbox
    val designParamDclCS =
      // for qsys blackbox, we extend the base IP class with its parameters and declare no new parameters
      if (designIsQsysIPBlackbox) ""
      else
        if (designParamList.length == 0) ""
        else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
        else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val designParamInstCS =
      // for qsys blackbox, we define the parameters in the class extension instead of the
      // blackbox instantiation
      if (designIsQsysIPBlackbox) csDFDesignBlockParamInst(design)
      else ""
    val dcl =
      s"class ${design.dclName}$designParamDclCS extends $dsnCls$designParamInstCS"
    val dclWithBody =
      if (bodyWithDcls.isEmpty || designIsQsysIPBlackbox) dcl
      else s"$dcl:\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    val annotations =
      if (design.isTop) design.dclMeta.annotations ++ design.meta.annotations
      else design.dclMeta.annotations
    s"${printer.csAnnotations(annotations)}$dclWithBody\n"
  end csDFDesignBlockDcl
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFDesignLateBody(design)
    val designParamList = design.members(MemberView.Folded).collect { case param: DesignParam =>
      s"${param.getName} = ${param.dfValRef.refCodeString}"
    }
    val designParamCS = design.instMode match
      // for qsys blackbox, we define the parameters in the class extension instead of the
      // blackbox instantiation
      case InstMode.BlackBox(_: InstMode.BlackBox.Source.Qsys) => "()"
      case _                                                   => csDFDesignBlockParamInst(design)
    val inst =
      if (body.isEmpty) s"${design.dclName}$designParamCS"
      else s"new ${design.dclName}$designParamCS:\n${body.hindent}"
    s"val ${design.getName} = ${inst}"
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
      .map(bindVal => s"$${${bindVal.getName}: B[${bindVal.dfType.width}]}")
    val fullTerm = pattern.parts.coalesce(csBinds).mkString
    s"""${pattern.op}"$fullTerm""""
  def csDFCasePatternNamedArg(pattern: Pattern.NamedArg): String =
    s"${pattern.name} = ${csDFCasePattern(pattern.pattern)}"
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String =
    s" if ${guardRef.refCodeString}"
  def csDFCaseKeyword: String = "case "
  def csDFCaseSeparator: String = " =>"
  def csDFMatchEnd: String = "end match"
  def csDFMatchStatement(csSelector: String, wildcardSupport: Boolean): String =
    s"$csSelector match"
  def csProcessBlock(pb: ProcessBlock): String =
    val body = csDFOwnerBody(pb)
    val named = pb.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All                        => "(all)"
      case Sensitivity.List(refs) if refs.isEmpty => ""
      case Sensitivity.List(refs)                 => refs.map(_.refCodeString).mkStringBrackets
    s"${named}process${senList}:\n${body.hindent}"
  def csStepBlock(stepBlock: StepBlock): String =
    val body = csDFOwnerBody(stepBlock)
    val name = stepBlock.getName
    val defType = if (stepBlock.isRegular) "Step" else "Unit"
    s"def $name: $defType =\n${body.hindent}\nend $name"
  def csDFForBlock(forBlock: DFLoop.DFForBlock): String =
    val csCOMB_LOOP = if (forBlock.isCombinational) "COMB_LOOP" else ""
    val body =
      sn"""|${csCOMB_LOOP}
           |${csDFOwnerBody(forBlock)}"""
    val named = forBlock.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    //format: off
    sn"""|${named}for (${forBlock.iteratorRef.refCodeString} <- ${printer.csDFRange(forBlock.rangeRef.get)})
         |${body.hindent}
         |end for"""
    //format: on
  def csDFWhileBlock(whileBlock: DFLoop.DFWhileBlock): String =
    val csCOMB_LOOP = if (whileBlock.isCombinational) "COMB_LOOP" else ""
    val body =
      sn"""|${csCOMB_LOOP}
           |${csDFOwnerBody(whileBlock)}"""
    val named = whileBlock.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    sn"""|${named}while (${whileBlock.guardRef.refCodeString})
         |${body.hindent}
         |end while"""
  def csDomainBlock(domain: DomainBlock): String =
    val body = csDFOwnerBody(domain)
    val named = domain.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val domainStr = domain.domainType match
      case DomainType.DF     => "DFDomain"
      case rt: DomainType.RT =>
        rt.cfg match
          case RTDomainCfg.Related(relatedDomainRef) =>
            val relatedDomain = relatedDomainRef.get
            if (domain.isMemberOf(relatedDomain))
              "RelatedDomain"
            else
              s"${relatedDomain.getRelativeName(domain.getOwnerNamed)}.RelatedDomain"
          case RTDomainCfg.Derived => "RTDomain"
          case _                   =>
            s"RTDomain(${printer.csRTDomainCfg(rt.cfg)})"
      case DomainType.ED => "EDDomain"
    sn"""|${named}new $domainStr:
         |${body.hindent}"""
  end csDomainBlock

end DFOwnerPrinter
