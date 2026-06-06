package dfhdl.options

import BuilderOptions.*
import dfhdl.core.Design

final case class BuilderOptions(
    onError: _OnError,
    Werror: WError,
    tool: _Tool,
    flash: Flash,
    compress: Compress
) extends ToolOptions

//defaults common for all linting tools
object BuilderOptions:
  opaque type Defaults[-T] <: BuilderOptions = BuilderOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        tool: Tool,
        flash: Flash,
        compress: Compress
    ): Defaults[Any] =
      BuilderOptions(
        onError = onError(dfhdl.options.OnError),
        Werror = Werror, tool = tool(dfhdl.tools.builders), flash = flash, compress = compress
      )
  given (using defaults: Defaults[Design]): BuilderOptions = defaults

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

  type Tool = dfhdl.tools.builders.type => _Tool
  private[dfhdl] into opaque type _Tool <: dfhdl.tools.builders = dfhdl.tools.builders
  object _Tool:
    export dfhdl.tools.builders.{foss, vendor}
    given Tool = _.vendor
    given Conversion[dfhdl.tools.builders, _Tool] = identity

  into opaque type Flash <: Boolean = Boolean
  object Flash:
    given Flash = false
    given Conversion[Boolean, Flash] = identity

  into opaque type Compress <: Boolean = Boolean
  object Compress:
    given Compress = true
    given Conversion[Boolean, Compress] = identity
end BuilderOptions
