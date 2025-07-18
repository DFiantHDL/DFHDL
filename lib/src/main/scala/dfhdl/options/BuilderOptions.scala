package dfhdl.options

import BuilderOptions.*
import dfhdl.core.Design

final case class BuilderOptions(
    onError: OnError,
    Werror: WError
) extends ToolOptions

//defaults common for all linting tools
object BuilderOptions:
  opaque type Defaults[-T <: Design] <: BuilderOptions = BuilderOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError
    ): Defaults[Design] = BuilderOptions(onError = onError, Werror = Werror)
  given (using defaults: Defaults[Design]): BuilderOptions = defaults

  opaque type OnError <: dfhdl.options.ToolOptions.OnError = dfhdl.options.ToolOptions.OnError
  object OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WError <: dfhdl.options.ToolOptions.WError = dfhdl.options.ToolOptions.WError
  object WError:
    given (using Werror: dfhdl.options.ToolOptions.WError): WError = Werror
    given [T](using conv: Conversion[T, dfhdl.options.ToolOptions.WError]): Conversion[T, WError] =
      t => conv(t).asInstanceOf[WError]
end BuilderOptions
