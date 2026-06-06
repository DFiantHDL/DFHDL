package dfhdl.options

trait ToolOptions:
  val onError: ToolOptions._OnError
  val Werror: ToolOptions.WError

//defaults common for all tools
object ToolOptions:
  type OnError = dfhdl.options.OnError.type => _OnError
  private[dfhdl] into opaque type _OnError <: dfhdl.options._OnError = dfhdl.options._OnError
  object _OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options._OnError, _OnError] = x => x.asInstanceOf[_OnError]

  into opaque type WError <: dfhdl.options.WError = dfhdl.options.WError
  object WError:
    given (using werror: dfhdl.options.WError): WError = werror
    given [T](using conv: Conversion[T, dfhdl.options.WError]): Conversion[T, WError] = t =>
      conv(t).asInstanceOf[WError]
    given Conversion[dfhdl.options.WError, WError] = identity
end ToolOptions
