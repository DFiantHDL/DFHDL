package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

type DFOwner = OpaqueDFOwner.DFOwner
val DFOwner = OpaqueDFOwner.DFOwner

private object OpaqueDFOwner:
  opaque type DFOwner <: DFMember.Of[ir.DFOwner] = DFMember.Of[ir.DFOwner]
  object DFOwner:
    extension (owner: ir.DFOwner)
      def asFE: DFOwner = owner.asInstanceOf[DFOwner]
