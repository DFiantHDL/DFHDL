package dfhdl.options

trait ToolOptions:
  val onError: ToolOptions.OnError
  val Werror: ToolOptions.WError

//defaults common for all tools
object ToolOptions:
  opaque type OnError <: dfhdl.options.OnError = dfhdl.options.OnError
  object OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = identity
    export dfhdl.options.OnError.*

  opaque type WError <: dfhdl.options.WError = dfhdl.options.WError
  object WError:
    given (using werror: dfhdl.options.WError): WError = werror
    given [T](using conv: Conversion[T, dfhdl.options.WError]): Conversion[T, WError] = t =>
      conv(t).asInstanceOf[WError]
    given Conversion[dfhdl.options.WError, WError] = identity
end ToolOptions
