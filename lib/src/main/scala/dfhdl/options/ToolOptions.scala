package dfhdl.options

trait ToolOptions:
  val onError: ToolOptions._OnError
  val Werror: ToolOptions.WError
  val location: ToolOptions._Location
  // the selected run location (DFTools image or local PATH), for use by the tools layer
  final def runLocation: ToolOptions._Location = location

//defaults common for all tools
object ToolOptions:
  type OnError = dfhdl.options.OnError.type => _OnError
  private[dfhdl] into opaque type _OnError <: dfhdl.options._OnError = dfhdl.options._OnError
  object _OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options._OnError, _OnError] = x => x.asInstanceOf[_OnError]

  // where the external tools are run from; defaults to the DFTools image.
  //   dftools - the pinned DFTools Apptainer image (default)
  //   local   - tools discovered on the system PATH
  type Location = _Location.type => _Location
  val Location = _Location
  protected[dfhdl] enum _Location derives CanEqual:
    case dftools, local
  object _Location:
    // TODO: switch the default to `dftools` once a DFTools image release is published
    given Location = _ => local

  into opaque type WError <: dfhdl.options.WError = dfhdl.options.WError
  object WError:
    given (using werror: dfhdl.options.WError): WError = werror
    given [T](using conv: Conversion[T, dfhdl.options.WError]): Conversion[T, WError] = t =>
      conv(t).asInstanceOf[WError]
    given Conversion[dfhdl.options.WError, WError] = identity
end ToolOptions
