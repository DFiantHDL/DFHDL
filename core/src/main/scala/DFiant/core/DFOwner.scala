package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

opaque type DFOwner = ir.DFOwner
extension (owner: ir.DFOwner) def asFE: DFOwner = owner
object DFOwner:
  extension (owner: DFOwner) def asIR: ir.DFOwner = owner
