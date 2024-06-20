package dfhdl.options

import GHDLOptions.*
import dfhdl.tools.toolsCore.VHDLLinterOptions

final case class GHDLOptions(
    onError: OnError,
    warnAsError: WarnAsError
) extends VHDLLinterOptions

object GHDLOptions:
  given default(using
      onError: OnError,
      warnAsError: WarnAsError
  ): GHDLOptions = GHDLOptions(onError = onError, warnAsError = warnAsError)

  opaque type OnError <: LinterOptions.OnError = LinterOptions.OnError
  object OnError:
    given (using onError: LinterOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WarnAsError <: LinterOptions.WarnAsError = LinterOptions.WarnAsError
  object WarnAsError:
    given Conversion[LinterOptions.WarnAsError, WarnAsError] = identity
    given (using warnAsError: LinterOptions.WarnAsError): WarnAsError = warnAsError
end GHDLOptions
