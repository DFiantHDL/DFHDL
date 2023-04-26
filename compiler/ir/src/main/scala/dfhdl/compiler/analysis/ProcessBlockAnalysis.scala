package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import ir.ProcessBlock.Sensitivity

extension (pb: ProcessBlock)(using MemberGetSet)
  def isSequential: Boolean =
    pb.sensitivity match
      case Sensitivity.All        => false
      case Sensitivity.List(refs) => true // TODO: fix this
