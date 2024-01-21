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
        case DesignParam(_) => false
        // an ident placeholder (can be anonymous)
        case Ident(_) => true
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
        case DFNet.Connection(PortOfDesignDef(Modifier.IN, _), _, _) =>
          false
        // include the rest of the nets
        case net: DFNet => true
        // including only conditional statements (no type) headers
        case ch: DFConditional.Header => ch.dfType == DFUnit
        // process blocks
        case pb: ProcessBlock => true
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .toList
      .emptyOr(_.mkString("\n"))
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
  def csDFIfStatement(csCond: String): String
  def csDFElseStatement: String
  def csDFElseIfStatement(csCond: String): String
  final def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => csDFIfStatement(ifBlock.guardRef.refCodeString)
      case _ =>
        ifBlock.guardRef.get match
          case DFMember.Empty => csDFElseStatement
          case _              => csDFElseIfStatement(ifBlock.guardRef.refCodeString)
  def csDFIfEnd(lastCB: DFConditional.DFIfElseBlock): String
  def csIfBlockEmpty: String
  def csDFCaseBlockEmpty: String
  def csDFCasePatternCatchAll: String
  def csDFCasePatternAlternativeToken: String
  def csDFCasePatternStruct(pattern: Pattern.Struct): String
  def csDFCasePatternBind(pattern: Pattern.Bind): String
  def csDFCasePatternBindSI(pattern: Pattern.BindSI): String
  def csDFCasePattern(pattern: Pattern): String = pattern match
    case Pattern.CatchAll         => csDFCasePatternCatchAll
    case Pattern.Singleton(token) => printer.csDFToken(token)
    case Pattern.Alternative(list) =>
      list.map(csDFCasePattern).mkString(csDFCasePatternAlternativeToken)
    case pattern: Pattern.Struct => csDFCasePatternStruct(pattern)
    case pattern: Pattern.Bind   => csDFCasePatternBind(pattern)
    case pattern: Pattern.BindSI => csDFCasePatternBindSI(pattern)
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String
  def csDFCaseKeyword: String
  def csDFCaseSeparator: String
  final def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String =
    val csGuard =
      caseBlock.guardRef.get match
        case DFMember.Empty => ""
        case _              => csDFCaseGuard(caseBlock.guardRef)
    s"$csDFCaseKeyword${csDFCasePattern(caseBlock.pattern)}$csGuard$csDFCaseSeparator"
  def csDFMatchStatement(csSelector: String): String
  def csDFMatchEnd: String
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
      if (body.contains("\n"))
        s"${csBlockBegin.emptyOr(" " + _)}\n${body.hindent}${csBlockEnd.emptyOr("\n" + _)}"
      else s" $body"
    if (body.isEmpty) cb match
      case caseBlock: DFConditional.DFCaseBlock => s"$statement$csDFCaseBlockEmpty"
      case ifBlock: DFConditional.DFIfElseBlock => s"$statement$csIfBlockEmpty"
    else s"$statement$indentBody${end.emptyOr(e => s"\n$e")}"
  end csDFConditionalBlock
  final def csDFConditional(ch: DFConditional.Header): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString.applyBrackets()
        s"${csDFMatchStatement(csSelector)}\n${csChains.hindent}${csDFMatchEnd.emptyOr(e => s"\n$e")}"
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

    val retDFType = retValOpt.map(_.dfType).getOrElse(DFUnit)
    val retTypeCS = s": ${printer.csDFType(retDFType, typeCS = true)} <> DFRET"
    val dcl =
      s"def ${design.dclName}($defArgsCS)$retTypeCS =\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    s"${printer.csAnnotations(design.dclMeta)}$dcl\n"
  end csDFDesignDefDcl
  def csDFDesignDefInst(design: DFDesignBlock): String =
    val ports = design.members(MemberView.Folded).view.collect { case port @ DclIn() =>
      val DFNet.Connection(_, from: DFVal, _) = port.getConnectionTo.get: @unchecked
      printer.csDFValRef(from, design.getOwner)
    }.mkString(", ")
    val dcl = s"${design.dclName}($ports)"
    if (design.isAnonymous) dcl
    else s"val ${design.getName} = $dcl"
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    import design.instMode
    val localDcls = printer.csLocalTypeDcls(design)
    val body = csDFOwnerBody(design)
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    val dsnCls = design.domainType match
      case DomainType.DF => "DFDesign"
      case rt: DomainType.RT =>
        val cfgStr = rt.cfg match
          case _: DerivedCfg.type => ""
          case _                  => s"(${printer.csRTDomainCfg(rt.cfg)})"
        s"""RTDesign$cfgStr""".stripMargin
      case _ => "EDDesign"
    val designParamList = design.members(MemberView.Folded).collect { case param @ DesignParam(_) =>
      val defaultValue = if (design.isTop) s" = ${param.relValRef.refCodeString}" else ""
      s"${param.getName}${printer.csDFValConstType(param.dfType)}$defaultValue"
    }
    val designParamCS =
      if (designParamList.length == 0) ""
      else if (designParamList.length == 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
    val dcl = s"class ${design.dclName}$designParamCS extends $dsnCls"
    val dclWithBody =
      if (bodyWithDcls.isEmpty) dcl else s"$dcl:\n${bodyWithDcls.hindent}\nend ${design.dclName}"
    s"${printer.csAnnotations(design.dclMeta)}$dclWithBody\n"
  end csDFDesignBlockDcl
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFDesignLateBody(design)
    val designParamList = design.members(MemberView.Folded).collect { case param @ DesignParam(_) =>
      s"${param.getName} = ${param.relValRef.refCodeString}"
    }
    val designParamCS =
      if (designParamList.length <= 1) designParamList.mkString("(", ", ", ")")
      else "(" + designParamList.mkString("\n", ",\n", "\n").hindent(2) + ")"
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
        case net: DFNet =>
          if (hasNet) break(true)
          hasNet = true
        case _ =>
      }
      false
    }
    if (lastCB.getLeadingChain.exists(isBigBlock)) "end if" else ""
  end csDFIfEnd
  def csIfBlockEmpty: String = " {}"
  def csDFCaseBlockEmpty: String = ""
  def csDFCasePatternCatchAll: String = "_"
  def csDFCasePatternAlternativeToken: String = " | "
  def csDFCasePatternStruct(pattern: Pattern.Struct): String =
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
  def csDFCaseGuard(guardRef: DFConditional.Block.GuardRef): String =
    s" if ${guardRef.refCodeString}"
  def csDFCaseKeyword: String = "case "
  def csDFCaseSeparator: String = " =>"
  def csDFMatchEnd: String = "end match"
  def csDFMatchStatement(csSelector: String): String = s"$csSelector match"
  def csProcessBlock(pb: ProcessBlock): String =
    val body = csDFOwnerBody(pb)
    val named = pb.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val senList = pb.sensitivity match
      case Sensitivity.All                        => "(all)"
      case Sensitivity.List(refs) if refs.isEmpty => ".forever"
      case Sensitivity.List(refs)                 => refs.map(_.refCodeString).mkStringBrackets
    s"${named}process${senList}:\n${body.hindent}"
  def csDomainBlock(domain: DomainBlock): String =
    val body = csDFOwnerBody(domain)
    val named = domain.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val domainStr = domain.domainType match
      case DomainType.DF => "DFDomain"
      case rt: DomainType.RT =>
        val cfgStr = rt.cfg match
          case _: DerivedCfg.type => ""
          case _                  => s"(${printer.csRTDomainCfg(rt.cfg)})"
        s"RTDomain$cfgStr".stripMargin
      case DomainType.ED => "EDDomain"
    s"${named}new $domainStr:\n${body.hindent}"

end DFOwnerPrinter
