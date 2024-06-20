package dfhdl.options

import LinterOptions.*

trait LinterOptions extends ToolOptions:
  val onError: OnError
  val warnAsError: WarnAsError

//defaults common for all linting tools
object LinterOptions:
  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WarnAsError <: Boolean = Boolean
  object WarnAsError:
    given WarnAsError = false
    given Conversion[Boolean, WarnAsError] = identity
