package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

final class DFOwner[+T <: ir.DFOwner](val irValue: T | DFError) extends AnyVal with DFMember[T]
object DFOwner:
  extension [T <: ir.DFOwner](owner: T) def asFE: DFOwner[T] = DFOwner(owner)

type DFOwnerAny = DFOwner[ir.DFOwner]
