package dfhdl.options

trait ToolOptions:
  val onError: ToolOptions.OnError

//defaults common for all tools
object ToolOptions:
  opaque type OnError <: dfhdl.options.OnError = dfhdl.options.OnError
  object OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = identity
    export dfhdl.options.OnError.*
end ToolOptions
