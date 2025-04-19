package dfhdl.options
import dfhdl.core.Wait.Duration
import dfhdl.core.Wait.Ops.cy
import dfhdl.core.Design
import SimulatorOptions.*

final case class SimulatorOptions(
    onError: OnError,
    Werror: WError,
    verilogSimulator: VerilogSimulator,
    vhdlSimulator: VHDLSimulator,
    runLimit: RunLimit
) extends ToolOptions

//defaults common for all linting tools
object SimulatorOptions:
  opaque type Defaults[-T <: Design] <: SimulatorOptions = SimulatorOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        verilogSimulator: VerilogSimulator,
        vhdlSimulator: VHDLSimulator,
        runLimit: RunLimit
    ): Defaults[Design] = SimulatorOptions(
      onError = onError, Werror = Werror, verilogSimulator = verilogSimulator,
      vhdlSimulator = vhdlSimulator, runLimit = runLimit
    )
  given (using defaults: Defaults[Design]): SimulatorOptions = defaults

  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WError <: dfhdl.options.ToolOptions.WError = dfhdl.options.ToolOptions.WError
  object WError:
    given (using Werror: dfhdl.options.ToolOptions.WError): WError = Werror
    given [T](using conv: Conversion[T, dfhdl.options.ToolOptions.WError]): Conversion[T, WError] =
      t => conv(t).asInstanceOf[WError]

  opaque type VerilogSimulator <: dfhdl.tools.toolsCore.VerilogSimulator =
    dfhdl.tools.toolsCore.VerilogSimulator
  object VerilogSimulator:
    export dfhdl.tools.simulators.{verilator, iverilog, vlog, xvlog}
    given VerilogSimulator = verilator
    given Conversion[dfhdl.tools.toolsCore.VerilogSimulator, VerilogSimulator] = identity

  opaque type VHDLSimulator <: dfhdl.tools.toolsCore.VHDLSimulator =
    dfhdl.tools.toolsCore.VHDLSimulator
  object VHDLSimulator:
    export dfhdl.tools.simulators.{ghdl, nvc, vcom, xvhdl}
    given VHDLSimulator = ghdl
    given Conversion[dfhdl.tools.toolsCore.VHDLSimulator, VHDLSimulator] = identity

  opaque type RunLimit <: (Duration | None.type) = (Duration | None.type)
  object RunLimit:
    given RunLimit = 100.cy
    given Conversion[Duration | None.type, RunLimit] = identity
end SimulatorOptions
