package dfhdl.options

import LinterOptions.*

final case class LinterOptions(
    onError: OnError,
    warnAsError: WarnAsError,
    verilogLinter: VerilogLinter,
    vhdlLinter: VHDLLinter
) extends ToolOptions

//defaults common for all linting tools
object LinterOptions:
  given default(using
      onError: OnError,
      warnAsError: WarnAsError,
      verilogLinter: VerilogLinter,
      vhdlLinter: VHDLLinter
  ): LinterOptions = LinterOptions(
    onError = onError,
    warnAsError = warnAsError,
    verilogLinter = verilogLinter,
    vhdlLinter = vhdlLinter
  )

  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WarnAsError <: Boolean = Boolean
  object WarnAsError:
    given WarnAsError = false
    given Conversion[Boolean, WarnAsError] = identity

  opaque type VerilogLinter <: dfhdl.tools.toolsCore.VerilogLinter =
    dfhdl.tools.toolsCore.VerilogLinter
  object VerilogLinter:
    export dfhdl.tools.linters.{verilator, iverilog}
    given VerilogLinter = verilator
    given Conversion[dfhdl.tools.toolsCore.VerilogLinter, VerilogLinter] = identity

  opaque type VHDLLinter <: dfhdl.tools.toolsCore.VHDLLinter = dfhdl.tools.toolsCore.VHDLLinter
  object VHDLLinter:
    export dfhdl.tools.linters.{ghdl, nvc}
    given VHDLLinter = ghdl
    given Conversion[dfhdl.tools.toolsCore.VHDLLinter, VHDLLinter] = identity

end LinterOptions
