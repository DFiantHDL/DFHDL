package DFiant.compiler
package printing
import ir.*
import analysis.*
import DFiant.internals.*
import DFVal.*

protected trait DFOwnerPrinter extends AbstractPrinter:
  private def csDFOwnerBody(owner: DFOwner, lateConstruction: Boolean)(using
      MemberGetSet
  ): String = csDFMembers(owner.members, lateConstruction)
  private[DFiant] def csDFMembers(
      members: List[DFMember],
      lateConstruction: Boolean
  )(using
      MemberGetSet
  ): String =
    members.view
      //only members that match the requested construction mode
      .filter(m => m.hasLateConstruction == lateConstruction)
      //only members that are named or nets
      .collect {
        case m: DFMember.Named if !m.isAnonymous => m
        case net: DFNet                          => net
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
  def csDFConditionalBlock(cb: DFConditionalBlock)(using MemberGetSet): String =
    ""
  def csDFOwner(owner: DFOwner)(using MemberGetSet): String = owner match
    case design: DFDesignBlock  => csDFDesignBlockInst(design)
    case cb: DFConditionalBlock => csDFConditionalBlock(cb)
end DFOwnerPrinter
