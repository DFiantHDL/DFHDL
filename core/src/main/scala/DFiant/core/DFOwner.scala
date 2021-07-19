package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

opaque type DFOwner <: DFMember.Of[ir.DFOwner] = DFMember.Of[ir.DFOwner]
object DFOwner:
  extension (owner: ir.DFOwner) def asFE: DFOwner = owner.asInstanceOf[DFOwner]
