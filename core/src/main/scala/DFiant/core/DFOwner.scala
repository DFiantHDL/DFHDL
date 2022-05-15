package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

class DFOwner[+T <: ir.DFOwner](val irValue: T | DFError) extends DFMember[T] //AnyVal with
object DFOwner:
  extension [T <: ir.DFOwner](owner: T) def asFE: DFOwner[T] = DFOwner(owner)

type DFOwnerAny = DFOwner[ir.DFOwner]
