package dfhdl.options

import LinterOptions.*

final case class LinterOptions(
    onError: OnError,
    fatalWarnings: FatalWarnings,
    verilogLinter: VerilogLinter,
    vhdlLinter: VHDLLinter
) extends ToolOptions

//defaults common for all linting tools
object LinterOptions:
  given default(using
      onError: OnError,
      fatalWarnings: FatalWarnings,
      verilogLinter: VerilogLinter,
      vhdlLinter: VHDLLinter
  ): LinterOptions = LinterOptions(
    onError = onError,
    fatalWarnings = fatalWarnings,
    verilogLinter = verilogLinter,
    vhdlLinter = vhdlLinter
  )

  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type FatalWarnings <: dfhdl.options.ToolOptions.FatalWarnings =
    dfhdl.options.ToolOptions.FatalWarnings
  object FatalWarnings:
    given (using fatalWarnings: dfhdl.options.ToolOptions.FatalWarnings): FatalWarnings =
      fatalWarnings
    given Conversion[Boolean, FatalWarnings] = x => x.asInstanceOf[FatalWarnings]
    given Conversion[dfhdl.options.ToolOptions.FatalWarnings, FatalWarnings] =
      x => x.asInstanceOf[FatalWarnings]

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
