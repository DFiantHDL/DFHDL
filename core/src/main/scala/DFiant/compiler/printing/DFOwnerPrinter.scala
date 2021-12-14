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
      // only members that are named, nets, or conditionals
      .collect {
        case m: DFMember.Named if !m.isAnonymous => m
        case net: DFNet                          => net
        case ifBlock: DFIfElseBlock              => ifBlock
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
  def csDFIfElseBlock(ifBlock: DFIfElseBlock)(using MemberGetSet): String =
    val body = csDFOwnerBody(ifBlock, false)
    val statement = ifBlock.prevBlockRef match
      case _: DFRef.Empty => s"if (${ifBlock.condRef.refCodeString})"
      case _ =>
        ifBlock.condRef match
          case _: DFRef.Empty => s"else"
          case _              => s"else if (${ifBlock.condRef.refCodeString})"
    if (body.isEmpty) s"$statement {}" else s"$statement\n${body.delim(1)}"
  def csDFOwner(owner: DFOwner)(using MemberGetSet): String = owner match
    case design: DFDesignBlock  => csDFDesignBlockInst(design)
    case ifBlock: DFIfElseBlock => csDFIfElseBlock(ifBlock)
end DFOwnerPrinter
