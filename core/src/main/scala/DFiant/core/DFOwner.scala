package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

class DFOwner(val value: ir.DFOwner) extends AnyVal with DFMember[ir.DFOwner]
object DFOwner:
  extension (owner: ir.DFOwner) def asFE: DFOwner = DFOwner(owner)
