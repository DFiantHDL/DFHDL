package dfhdl.options

import LinterOptions.*

trait LinterOptions extends ToolOptions:
  val onError: OnError
  val warnAsError: WarnAsError

//defaults common for all linting tools
object LinterOptions:
  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    export dfhdl.options.OnError.*

  opaque type WarnAsError <: Boolean = Boolean
  given Conversion[Boolean, WarnAsError] = identity
  object WarnAsError:
    given WarnAsError = false
