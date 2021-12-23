package DFiant.compiler
package printing
import ir.*
import analysis.*
import DFiant.internals.*
import DFVal.*
protected trait DFOwnerPrinter extends AbstractPrinter:
  private def csDFOwnerBody(owner: DFOwner, lateConstruction: Boolean)(using
      MemberGetSet
  ): String = csDFMembers(owner.members(MemberView.Folded), lateConstruction)
  private[DFiant] def csDFMembers(
      members: List[DFMember],
      lateConstruction: Boolean
  )(using
      MemberGetSet
  ): String =
    members.view
      // only members that match the requested construction mode
      .filter(m => m.hasLateConstruction == lateConstruction)
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
  def csDFDesignBlockDcl(design: DFDesignBlock)(using MemberGetSet): String =
    val body = csDFOwnerBody(design, false)
    val dcl = s"class ${design.designType}(using DFC) extends DFDesign"
    if (body.isEmpty) dcl else s"$dcl:\n${body.indent(1)}"
  def csDFDesignBlockInst(design: DFDesignBlock)(using MemberGetSet): String =
    val body = csDFOwnerBody(design, true)
    val inst = s"new ${design.designType}"
    if (body.isEmpty) inst else s"$inst:\n${body.indent(1)}"
  def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock)(using
      MemberGetSet
  ): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => s"if (${ifBlock.condRef.refCodeString})"
      case _ if ifBlock.condRef.isEmpty => s"else"
      case _ => s"else if (${ifBlock.condRef.refCodeString})"
  def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock)(using
      MemberGetSet
  ): String =
    ???
  def csDFConditionalBlock(cb: DFConditional.Block)(using
      MemberGetSet
  ): String =
    val body = csDFOwnerBody(cb, false)
    val statement = cb match
      case caseBlock: DFConditional.DFCaseBlock => csDFCaseStatement(caseBlock)
      case ifBlock: DFConditional.DFIfElseBlock => csDFIfElseStatement(ifBlock)
    val indentBody =
      if (body.contains("\n")) s"\n${body.indent()}" else s" $body"
    if (body.isEmpty) s"$statement {}" else s"$statement$indentBody"
  def csDFConditional(ch: DFConditional.Header)(using
      MemberGetSet
  ): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString.applyBrackets()
        s"$csSelector match\n${csChains.indent()}"
      case ih: DFConditional.DFIfHeader => csChains
  end csDFConditional
end DFOwnerPrinter
