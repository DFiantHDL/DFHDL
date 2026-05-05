package dfhdl.compiler.ir
import dfhdl.internals.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.constraints.Timing

type ClkRstTiming = (Option[Timing.Clock], Option[Timing.Reset])
type ClkRstTimingMap = Map[DFDomainOwner, ClkRstTiming]
