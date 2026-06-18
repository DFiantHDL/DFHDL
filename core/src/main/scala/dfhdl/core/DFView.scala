package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import NamedTuple.AnyNamedTuple

type DFView[+I <: Interface, +F <: AnyNamedTuple] =
  DFType[ir.DFView, Args2[I @uncheckedVariance, F @uncheckedVariance]]
