package dfhdl.options
import dfhdl.core.Design
import LinterOptions.*

final case class LinterOptions(
    onError: OnError,
    Werror: WError,
    verilogLinter: VerilogLinter,
    vhdlLinter: VHDLLinter
) extends ToolOptions

//defaults common for all linting tools
object LinterOptions:
  opaque type Defaults[-T <: Design] <: LinterOptions = LinterOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        verilogLinter: VerilogLinter,
        vhdlLinter: VHDLLinter
    ): Defaults[Design] = LinterOptions(
      onError = onError,
      Werror = Werror,
      verilogLinter = verilogLinter,
      vhdlLinter = vhdlLinter
    )
  given (using defaults: Defaults[Design]): LinterOptions = defaults

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

  opaque type VerilogLinter <: dfhdl.tools.toolsCore.VerilogLinter =
    dfhdl.tools.toolsCore.VerilogLinter
  object VerilogLinter:
    export dfhdl.tools.linters.{verilator, iverilog, vlog, xvlog}
    given VerilogLinter = verilator
    given Conversion[dfhdl.tools.toolsCore.VerilogLinter, VerilogLinter] = identity

  opaque type VHDLLinter <: dfhdl.tools.toolsCore.VHDLLinter = dfhdl.tools.toolsCore.VHDLLinter
  object VHDLLinter:
    export dfhdl.tools.linters.{ghdl, nvc, vcom, xvhdl}
    given VHDLLinter = ghdl
    given Conversion[dfhdl.tools.toolsCore.VHDLLinter, VHDLLinter] = identity

end LinterOptions
