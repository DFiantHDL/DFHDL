package dfhdl.core
import dfhdl.compiler.ir

val DFNothing = new DFType[ir.DFNothing, NoArgs](ir.DFNothing)
type DFNothing = DFType[ir.DFNothing, NoArgs]
