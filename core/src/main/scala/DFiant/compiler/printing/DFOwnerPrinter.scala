package DFiant.compiler
package printing
import ir.*
import analysis.*
import DFiant.internals.*
import DFVal.*
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern
protected trait DFOwnerPrinter extends AbstractPrinter:
  private def csDFOwnerBody(owner: DFOwner, lateConstruction: Boolean): String =
    csDFMembers(owner.members(MemberView.Folded), lateConstruction)
  def csDFMembers(
      members: List[DFMember],
      lateConstruction: Boolean
  ): String =
    members.view
      // only members that match the requested construction mode
      .filter {
        case n: DFNet => n.lateConstruction == lateConstruction
        case _        => !lateConstruction
      }
      // exclude bind members
      .filter {
        case Bind(_) => false
        case _       => true
      }
      // only members the following members are accepted:
      .collect {
        // an ident placeholder (can be anonymous)
        case m @ Ident(_) => m
        // named members
        case m: DFMember.Named if !m.isAnonymous => m
        // nets
        case net: DFNet => net
        // conditional headers
        case ch: DFConditional.Header if ch.dfType == NoType => ch
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    val localDcls =
      getSet.designDB
        .getLocalNamedDFTypes(design)
        .toList
        .sortBy(_.getName) // we sort the declarations by name, to have compilation consistency
        .map(printer.csNamedDFTypeDcl)
        .mkString("\n")
    val body = csDFOwnerBody(design, false)
    val bodyWithDcls = if (localDcls.isEmpty) body else s"$localDcls\n\n$body"
    val dsnCls = design.domain match
      case Domain.DF => "DFDesign"
      case _         => "RTDesign"
    val dcl = s"class ${design.dclName}(using DFC) extends $dsnCls"
    if (bodyWithDcls.isEmpty) dcl else s"$dcl:\n${bodyWithDcls.indent(1)}\nend ${design.dclName}"
  end csDFDesignBlockDcl
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerBody(design, true)
    val inst = s"val ${design.name} = new ${design.dclName}"
    if (body.isEmpty) inst else s"$inst:\n${body.indent(1)}"
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
    val body = csDFOwnerBody(cb, false)
    val statement = cb match
      case caseBlock: DFConditional.DFCaseBlock => csDFCaseStatement(caseBlock)
      case ifBlock: DFConditional.DFIfElseBlock => csDFIfElseStatement(ifBlock)
    val indentBody =
      if (body.contains("\n")) s"\n${body.indent()}" else s" $body"
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
        s"$csSelector match\n${csChains.indent()}"
      case ih: DFConditional.DFIfHeader => csChains
  end csDFConditional
end DFOwnerPrinter
