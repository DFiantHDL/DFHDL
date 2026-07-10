package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import ir.ProcessBlock.Sensitivity

extension (pb: ProcessBlock)
  def isInitial: Boolean =
    pb.sensitivity match
      case Sensitivity.Initial => true
      case _                   => false

extension (pb: ProcessBlock)(using MemberGetSet)
  def isSequential: Boolean =
    pb.sensitivity match
      case Sensitivity.All        => false
      case Sensitivity.Initial    => false
      case Sensitivity.List(refs) => true // TODO: fix this

extension (member: DFMember)(using MemberGetSet)
  def isInInitialBlock: Boolean = member.isOwnedCond(cond = {
    case pb: ProcessBlock => Some(pb.isInitial)
    case _: DFDomainOwner => Some(false)
    case _                => None
  })
