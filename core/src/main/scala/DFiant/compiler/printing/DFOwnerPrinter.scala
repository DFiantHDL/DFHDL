package DFiant.compiler
package printing
import ir.*
import analysis.*
import DFiant.internals.*
import DFVal.*
import DFiant.compiler.ir.AlwaysBlock.Sensitivity
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern

trait AbstractOwnerPrinter extends AbstractPrinter:
  final def csDFOwnerBody(owner: DFOwner): String =
    csDFMembers(owner.members(MemberView.Folded))
  final def csDFMembers(members: List[DFMember]): String =
    members.view
      // selecting viewable members:
      .filter {
        // excluding binds
        case Bind(_) => false
        // an ident placeholder (can be anonymous)
        case Ident(_) => true
        // named members
        case m: DFMember.Named if !m.isAnonymous => true
        // including only nets that are not lateConstruction
        case net: DFNet => !net.lateConstruction
        // including only conditional statements (no type) headers
        case ch: DFConditional.Header => ch.dfType == NoType
        // always blocks
        case ab: AlwaysBlock => true
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  final def csDFOwnerLateBody(owner: DFOwner): String =
    owner
      .members(MemberView.Folded)
      .view
      // selecting viewable members:
      .filter {
        // late construction nets
        case net: DFNet => net.lateConstruction
        // the rest are not directly viewable
        case _ => false
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  def csDFDesignBlockDcl(design: DFDesignBlock): String
  def csDFDesignBlockInst(design: DFDesignBlock): String
  def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String
  def csDFCasePattern(pattern: DFConditional.DFCaseBlock.Pattern): String
  def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String
  def csDFConditionalBlock(cb: DFConditional.Block): String
  def csDFConditional(ch: DFConditional.Header): String
  def csAlwaysBlock(ab: AlwaysBlock): String
  def csDomainBlock(ab: DomainBlock): String
end AbstractOwnerPrinter

protected trait DFOwnerPrinter extends AbstractOwnerPrinter:
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    val localDcls = printer.csLocalTypeDcls(design)
    val body = csDFOwnerBody(design)
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    val dsnCls = design.domainType match
      case DomainType.DF => "DFDesign"
      case _             => "RTDesign"
    val dcl = s"class ${design.dclName}(using DFC) extends $dsnCls"
    if (bodyWithDcls.isEmpty) dcl else s"$dcl:\n${bodyWithDcls.indent}\nend ${design.dclName}"
  end csDFDesignBlockDcl
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerLateBody(design)
    val inst = s"val ${design.name} = new ${design.dclName}"
    if (body.isEmpty) inst else s"$inst:\n${body.indent}"
  def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => s"if (${ifBlock.guardRef.refCodeString})"
      case _ =>
        ifBlock.guardRef.get match
          case DFMember.Empty => s"else"
          case _              => s"else if (${ifBlock.guardRef.refCodeString})"
  def csDFCasePattern(pattern: DFConditional.DFCaseBlock.Pattern): String = pattern match
    case Pattern.CatchAll => "_"
    case Pattern.Singleton(token) =>
      val csToken = printer.csDFToken(token)
      token match
        case DFEnum.Token(dt, data) => s"$csToken()"
        case _                      => csToken
    case Pattern.Alternative(list) =>
      list.map(csDFCasePattern).mkString(" | ")
    case Pattern.Struct(name, list) =>
      name + list.map(csDFCasePattern).mkStringBrackets
    case Pattern.Bind(ref, pattern) =>
      val bindStr = pattern match
        case Pattern.CatchAll => ""
        case _                => s" @ ${csDFCasePattern(pattern)}"
      s"${ref.get.name}$bindStr"
    case Pattern.BindSI(op, parts, refs) =>
      val csBinds = refs.view
        .map { r => r.get }
        .map(bindVal => s"$${${bindVal.name}: B[${bindVal.dfType.width}]}")
      val fullTerm = parts.coalesce(csBinds).mkString
      s"""$op"$fullTerm""""

  def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String =
    val csGuard =
      caseBlock.guardRef.get match
        case DFMember.Empty => ""
        case _              => s"if ${caseBlock.guardRef.refCodeString} "
    s"case ${csDFCasePattern(caseBlock.pattern)} ${csGuard}=>"
  def csDFConditionalBlock(cb: DFConditional.Block): String =
    val body = csDFOwnerBody(cb)
    val statement = cb match
      case caseBlock: DFConditional.DFCaseBlock => csDFCaseStatement(caseBlock)
      case ifBlock: DFConditional.DFIfElseBlock => csDFIfElseStatement(ifBlock)
    val indentBody =
      if (body.contains("\n")) s"\n${body.indent}" else s" $body"
    if (body.isEmpty) cb match
      case caseBlock: DFConditional.DFCaseBlock => statement
      case ifBlock: DFConditional.DFIfElseBlock => s"$statement {}"
    else s"$statement$indentBody"
  def csDFConditional(ch: DFConditional.Header): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString.applyBrackets()
        s"$csSelector match\n${csChains.indent}"
      case ih: DFConditional.DFIfHeader => csChains
  end csDFConditional
  def csAlwaysBlock(ab: AlwaysBlock): String =
    val body = csDFOwnerBody(ab)
    val named = ab.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val senList = ab.sensitivity match
      case Sensitivity.All        => ".all"
      case Sensitivity.List(refs) => refs.map(_.refCodeString).mkStringBrackets
    s"${named}always${senList} {\n${body.indent}\n}"
  def csDomainBlock(domain: DomainBlock): String =
    val body = csDFOwnerBody(domain)
    val named = domain.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val domainStr = domain.domainType match
      case df: DomainType.DF => "DFDomain"
      case rt: DomainType.RT => "RTDomain()"
    s"${named}new $domainStr:\n${body.indent}"

end DFOwnerPrinter
