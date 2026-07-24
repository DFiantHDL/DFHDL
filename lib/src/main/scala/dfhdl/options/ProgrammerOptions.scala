package dfhdl.options

import ProgrammerOptions.*
import dfhdl.core.Design

final case class ProgrammerOptions(
    onError: _OnError,
    Werror: WError,
    location: _Location,
    tool: _Tool,
    flash: Flash
) extends ToolOptions

//defaults common for all linting tools
object ProgrammerOptions:
  opaque type Defaults[-T] <: ProgrammerOptions = ProgrammerOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        location: Location,
        tool: Tool,
        flash: Flash
    ): Defaults[Any] =
      ProgrammerOptions(
        onError = onError(dfhdl.options.OnError),
        Werror = Werror,
        location = location(dfhdl.options.ToolOptions.Location),
        tool = tool(dfhdl.tools.programmers),
        flash = flash
      )
  end Defaults
  given (using defaults: Defaults[Design]): ProgrammerOptions = defaults

  type OnError = dfhdl.options.OnError.type => _OnError
  private[dfhdl] into opaque type _OnError <: dfhdl.options.ToolOptions._OnError =
    dfhdl.options.ToolOptions._OnError
  object _OnError:
    given (using onError: dfhdl.options.ToolOptions.OnError): OnError = onError
    given Conversion[dfhdl.options._OnError, _OnError] = x => x.asInstanceOf[_OnError]

  into opaque type WError <: dfhdl.options.ToolOptions.WError = dfhdl.options.ToolOptions.WError
  object WError:
    given (using Werror: dfhdl.options.ToolOptions.WError): WError = Werror
    given [T](using conv: Conversion[T, dfhdl.options.ToolOptions.WError]): Conversion[T, WError] =
      t => conv(t).asInstanceOf[WError]

  type Location = dfhdl.options.ToolOptions.Location.type => _Location
  private[dfhdl] into opaque type _Location <: dfhdl.options.ToolOptions._Location =
    dfhdl.options.ToolOptions._Location
  object _Location:
    given (using location: dfhdl.options.ToolOptions.Location): Location = location
    given Conversion[dfhdl.options.ToolOptions._Location, _Location] = x =>
      x.asInstanceOf[_Location]

  type Tool = dfhdl.tools.programmers.type => _Tool
  private[dfhdl] into opaque type _Tool <: dfhdl.tools.programmers = dfhdl.tools.programmers
  object _Tool:
    export dfhdl.tools.programmers.{foss, vendor}
    given Tool = _.vendor
    given Conversion[dfhdl.tools.programmers, _Tool] = identity

  into opaque type Flash <: Boolean = Boolean
  object Flash:
    given Flash = false
    given Conversion[Boolean, Flash] = identity
end ProgrammerOptions
