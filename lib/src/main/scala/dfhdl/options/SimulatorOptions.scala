package dfhdl.options
import dfhdl.core.Wait.Duration
import dfhdl.core.Wait.Ops.cy
import dfhdl.core.Design
import SimulatorOptions.*
import dfhdl.tools.toolsCore.Simulator
import dfhdl.backends

final case class SimulatorOptions(
    onError: _OnError,
    Werror: WError,
    verilogSimulator: _VerilogSimulator,
    vhdlSimulator: _VHDLSimulator,
    runLimit: RunLimit
) extends ToolOptions:
  def getTool(using co: CompilerOptions): Simulator =
    co.backend match
      case _: backends.verilog => verilogSimulator
      case _: backends.vhdl    => vhdlSimulator

//defaults common for all linting tools
object SimulatorOptions:
  opaque type Defaults[-T] <: SimulatorOptions = SimulatorOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        verilogSimulator: VerilogSimulator,
        vhdlSimulator: VHDLSimulator,
        runLimit: RunLimit
    ): Defaults[Any] = SimulatorOptions(
      onError = onError(dfhdl.options.OnError), Werror = Werror,
      verilogSimulator = verilogSimulator(dfhdl.tools.simulators.verilogSimulators),
      vhdlSimulator = vhdlSimulator(dfhdl.tools.simulators.vhdlSimulators), runLimit = runLimit
    )
  given (using defaults: Defaults[Design]): SimulatorOptions = defaults

  type OnError = dfhdl.options.OnError.type => _OnError
  private[dfhdl] into opaque type _OnError <: dfhdl.options.ToolOptions._OnError =
    dfhdl.options.ToolOptions._OnError
  private[dfhdl] object _OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options._OnError, _OnError] = x => x.asInstanceOf[_OnError]

  into opaque type WError <: dfhdl.options.ToolOptions.WError = dfhdl.options.ToolOptions.WError
  object WError:
    given (using Werror: dfhdl.options.ToolOptions.WError): WError = Werror
    given [T](using conv: Conversion[T, dfhdl.options.ToolOptions.WError]): Conversion[T, WError] =
      t => conv(t).asInstanceOf[WError]

  type VerilogSimulator = dfhdl.tools.simulators.verilogSimulators.type => _VerilogSimulator
  protected[dfhdl] into opaque type _VerilogSimulator <: dfhdl.tools.toolsCore.VerilogSimulator =
    dfhdl.tools.toolsCore.VerilogSimulator
  protected[dfhdl] object _VerilogSimulator:
    given VerilogSimulator = _.verilator
    given Conversion[dfhdl.tools.toolsCore.VerilogSimulator, _VerilogSimulator] = identity
    given Conversion[dfhdl.tools.simulators.questa.type, VerilogSimulator] = _ => _.vlog
    given Conversion[dfhdl.tools.simulators.vivado.type, VerilogSimulator] = _ => _.xvlog

  type VHDLSimulator = dfhdl.tools.simulators.vhdlSimulators.type => _VHDLSimulator
  protected[dfhdl] into opaque type _VHDLSimulator <: dfhdl.tools.toolsCore.VHDLSimulator =
    dfhdl.tools.toolsCore.VHDLSimulator
  protected[dfhdl] object _VHDLSimulator:
    given VHDLSimulator = _.ghdl
    given Conversion[dfhdl.tools.toolsCore.VHDLSimulator, _VHDLSimulator] = identity
    given Conversion[dfhdl.tools.simulators.questa.type, VHDLSimulator] = _ => _.vcom
    given Conversion[dfhdl.tools.simulators.vivado.type, VHDLSimulator] = _ => _.xvhdl

  into opaque type RunLimit <: (Duration | None.type) = (Duration | None.type)
  object RunLimit:
    given RunLimit = 100.cy
    given Conversion[Duration | None.type, RunLimit] = identity
end SimulatorOptions
