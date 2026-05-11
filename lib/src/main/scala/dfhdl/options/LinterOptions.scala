package dfhdl.options
import dfhdl.core.Design
import LinterOptions.*

final case class LinterOptions(
    onError: _OnError,
    Werror: WError,
    verilogLinter: _VerilogLinter,
    vhdlLinter: _VHDLLinter
) extends ToolOptions

//defaults common for all linting tools
object LinterOptions:
  opaque type Defaults[-T] <: LinterOptions = LinterOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        verilogLinter: VerilogLinter,
        vhdlLinter: VHDLLinter
    ): Defaults[Any] = LinterOptions(
      onError = onError(dfhdl.options.OnError),
      Werror = Werror,
      verilogLinter = verilogLinter(dfhdl.tools.linters.verilogLinters),
      vhdlLinter = vhdlLinter(dfhdl.tools.linters.vhdlLinters)
    )
  given (using defaults: Defaults[Design]): LinterOptions = defaults

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

  type VerilogLinter = dfhdl.tools.linters.verilogLinters.type => _VerilogLinter
  private[dfhdl] into opaque type _VerilogLinter <: dfhdl.tools.toolsCore.VerilogLinter =
    dfhdl.tools.toolsCore.VerilogLinter
  private[dfhdl] object _VerilogLinter:
    given VerilogLinter = _.verilator
    given Conversion[dfhdl.tools.toolsCore.VerilogLinter, _VerilogLinter] = identity

  type VHDLLinter = dfhdl.tools.linters.vhdlLinters.type => _VHDLLinter
  private[dfhdl] into opaque type _VHDLLinter <: dfhdl.tools.toolsCore.VHDLLinter =
    dfhdl.tools.toolsCore.VHDLLinter
  private[dfhdl] object _VHDLLinter:
    given VHDLLinter = _.ghdl
    given Conversion[dfhdl.tools.toolsCore.VHDLLinter, _VHDLLinter] = identity

end LinterOptions
