package dfhdl.options

import BuilderOptions.*
import dfhdl.core.Design

final case class BuilderOptions(
    onError: OnError,
    Werror: WError,
    tool: Tool,
    flash: Flash,
    compress: Compress
) extends ToolOptions

//defaults common for all linting tools
object BuilderOptions:
  opaque type Defaults[-T <: Design] <: BuilderOptions = BuilderOptions
  object Defaults:
    given (using
        onError: OnError,
        Werror: WError,
        tool: Tool,
        flash: Flash,
        compress: Compress
    ): Defaults[Design] =
      BuilderOptions(
        onError = onError, Werror = Werror, tool = tool, flash = flash, compress = compress
      )
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

  opaque type Tool <: dfhdl.tools.builders = dfhdl.tools.builders
  object Tool:
    export dfhdl.tools.builders.{foss, vendor}
    given Tool = dfhdl.tools.builders.vendor
    given Conversion[dfhdl.tools.builders, Tool] = identity

  opaque type Flash <: Boolean = Boolean
  object Flash:
    given Flash = false
    given Conversion[Boolean, Flash] = identity

  opaque type Compress <: Boolean = Boolean
  object Compress:
    given Compress = true
    given Conversion[Boolean, Compress] = identity
end BuilderOptions
