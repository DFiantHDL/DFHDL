package dfhdl.options

import ProgrammerOptions.*
import dfhdl.core.Design

final case class ProgrammerOptions(
    onError: OnError,
    Werror: WError,
    flash: Flash
) extends ToolOptions

//defaults common for all linting tools
object ProgrammerOptions:
  opaque type Defaults[-T <: Design] <: ProgrammerOptions = ProgrammerOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        flash: Flash
    ): Defaults[Design] = ProgrammerOptions(onError = onError, Werror = Werror, flash = flash)
  given (using defaults: Defaults[Design]): ProgrammerOptions = defaults

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

  opaque type Flash <: Boolean = Boolean
  object Flash:
    given Flash = false
    given Conversion[Boolean, Flash] = identity
end ProgrammerOptions
