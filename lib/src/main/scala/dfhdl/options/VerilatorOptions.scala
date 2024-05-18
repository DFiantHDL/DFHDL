package dfhdl.options

import VerilatorOptions.*
import dfhdl.tools.toolsCore.VerilogLinterOptions

final case class VerilatorOptions(
    onError: OnError,
    warnAsError: WarnAsError
) extends VerilogLinterOptions

object VerilatorOptions:
  given default(using
      onError: OnError,
      warnAsError: WarnAsError
  ): VerilatorOptions = VerilatorOptions(
    onError = onError,
    warnAsError = warnAsError
  )

  opaque type OnError <: LinterOptions.OnError = LinterOptions.OnError
  given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
  object OnError:
    given (using onError: LinterOptions.OnError): OnError = onError
    export dfhdl.options.OnError.*

  opaque type WarnAsError <: LinterOptions.WarnAsError = LinterOptions.WarnAsError
  given Conversion[LinterOptions.WarnAsError, WarnAsError] = identity
  object WarnAsError:
    given (using warnAsError: LinterOptions.WarnAsError): WarnAsError = warnAsError
end VerilatorOptions
