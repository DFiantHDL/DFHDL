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
        case m @ Placeholder() => m
        // named members
        case m: DFMember.Named if !m.isAnonymous => m
        // nets
        case net: DFNet => net
        // if headers
        case ifBlock: DFIfElseBlock if ifBlock.prevBlockRef.isEmpty => ifBlock
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  def csDFDesignBlockDcl(design: DFDesignBlock)(using MemberGetSet): String =
    val body = csDFOwnerBody(design, false)
    val dcl = s"class ${design.designType}(using DFC) extends DFDesign"
    if (body.isEmpty) dcl else s"$dcl:\n${body.delim(1)}"
  def csDFDesignBlockInst(design: DFDesignBlock)(using MemberGetSet): String =
    val body = csDFOwnerBody(design, true)
    val inst = s"new ${design.designType}"
    if (body.isEmpty) inst else s"$inst:\n${body.delim(1)}"
  def csDFIfElseBlockSingle(ifBlock: DFIfElseBlock)(using
      MemberGetSet
  ): String =
    val body = csDFOwnerBody(ifBlock, false)
    val statement =
      if (ifBlock.prevBlockRef.isEmpty) s"if (${ifBlock.condRef.refCodeString})"
      else if (ifBlock.condRef.isEmpty) s"else"
      else s"else if (${ifBlock.condRef.refCodeString})"
    val delimBody =
      if (body.contains("\n")) s"\n${body.delim(1)}" else s" $body"
    if (body.isEmpty) s"$statement {}" else s"$statement$delimBody"
  def csDFIfElseBlockChain(ifBlock: DFIfElseBlock)(using
      MemberGetSet
  ): String =
    val ifChains = getSet.designDB.ifChainTable
    val chain = ifChains(ifBlock)
    chain.map(ib => csDFIfElseBlockSingle(ib)).mkString("\n")
  def csDFOwner(owner: DFOwner)(using MemberGetSet): String = owner match
    case design: DFDesignBlock  => csDFDesignBlockInst(design)
    case ifBlock: DFIfElseBlock => csDFIfElseBlockChain(ifBlock)
end DFOwnerPrinter
