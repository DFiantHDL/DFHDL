package dfhdl.options
import dfhdl.core.{ClkCfg, RstCfg, Design}
import dfhdl.compiler.ir.RTDomainCfg
import dfhdl.core.DFPhysical.Val.Ops.MHz
import ElaborationOptions.*
final case class ElaborationOptions(
    logLevel: LogLevel,
    onError: OnError,
    Werror: WError,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg,
    printDFHDLCode: PrintDFHDLCode
):
  private[dfhdl] val defaultRTDomainCfg: RTDomainCfg.Explicit =
    RTDomainCfg.Explicit("RTDomainCfg.Default", defaultClkCfg, defaultRstCfg)
object ElaborationOptions:
  opaque type Defaults[-T <: Design] <: ElaborationOptions = ElaborationOptions
  object Defaults:
    given conv[T <: Design]: Conversion[ElaborationOptions, Defaults[T]] = identity
    given (using
        logLevel: LogLevel,
        onError: OnError,
        Werror: WError,
        defaultClkCfg: DefaultClkCfg,
        defaultRstCfg: DefaultRstCfg,
        printDFHDLCode: PrintDFHDLCode
    ): Defaults[Design] = ElaborationOptions(
      logLevel = logLevel, onError = onError, Werror = Werror, defaultClkCfg = defaultClkCfg,
      defaultRstCfg = defaultRstCfg, printDFHDLCode = printDFHDLCode
    )
  given defaults(using defaults: Defaults[Design]): ElaborationOptions = defaults

  opaque type LogLevel <: dfhdl.options.LogLevel = dfhdl.options.LogLevel
  object LogLevel:
    given Conversion[wvlet.log.LogLevel, LogLevel] = x => x.asInstanceOf[LogLevel]
    given (using logLevel: dfhdl.options.LogLevel): LogLevel = logLevel
    export dfhdl.options.LogLevel.*

  opaque type OnError <: dfhdl.options.OnError = dfhdl.options.OnError
  object OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  opaque type WError <: dfhdl.options.WError = dfhdl.options.WError
  object WError:
    given (using werror: dfhdl.options.WError): WError = werror
    given [T](using conv: Conversion[T, dfhdl.options.WError]): Conversion[T, WError] = t =>
      conv(t).asInstanceOf[WError]
    given Conversion[dfhdl.options.WError, WError] = identity

  opaque type DefaultClkCfg <: ClkCfg = ClkCfg
  object DefaultClkCfg:
    given default(using
        edge: Edge,
        rate: Rate,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): DefaultClkCfg = ClkCfg(edge, rate, portName, inclusionPolicy)
    given Conversion[ClkCfg, DefaultClkCfg] = identity
    given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]
    opaque type Edge <: ClkCfg.Edge = ClkCfg.Edge
    object Edge:
      given Edge = Edge.Rising
      given Conversion[ClkCfg.Edge, Edge] = identity
      export ClkCfg.Edge.*
    opaque type Rate <: ClkCfg.Rate = ClkCfg.Rate
    object Rate:
      given Rate = 50.MHz
      given Conversion[ClkCfg.Rate, Rate] = identity
    opaque type PortName <: String = String
    object PortName:
      given PortName = "clk"
      given Conversion[String, PortName] = identity
    opaque type InclusionPolicy <: ClkCfg.InclusionPolicy = ClkCfg.InclusionPolicy
    object InclusionPolicy:
      given InclusionPolicy = InclusionPolicy.AsNeeded
      given Conversion[ClkCfg.InclusionPolicy, InclusionPolicy] = identity
      export ClkCfg.InclusionPolicy.*
  end DefaultClkCfg

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  object DefaultRstCfg:
    given default(using
        mode: Mode,
        active: Active,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): DefaultRstCfg = RstCfg(mode, active, portName, inclusionPolicy)
    given Conversion[RstCfg, DefaultRstCfg] = identity
    given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]
    opaque type Mode <: RstCfg.Mode = RstCfg.Mode
    object Mode:
      given Mode = Mode.Sync
      given Conversion[RstCfg.Mode, Mode] = identity
      export RstCfg.Mode.*
    opaque type Active <: RstCfg.Active = RstCfg.Active
    object Active:
      given Active = Active.High
      given Conversion[RstCfg.Active, Active] = identity
      export RstCfg.Active.*
    opaque type PortName <: String = String
    object PortName:
      given PortName = "rst"
      given Conversion[String, PortName] = identity
    opaque type InclusionPolicy <: RstCfg.InclusionPolicy = RstCfg.InclusionPolicy
    object InclusionPolicy:
      given InclusionPolicy = InclusionPolicy.AsNeeded
      given Conversion[RstCfg.InclusionPolicy, InclusionPolicy] = identity
      export RstCfg.InclusionPolicy.*
  end DefaultRstCfg
  opaque type PrintDFHDLCode <: Boolean = Boolean
  object PrintDFHDLCode:
    given PrintDFHDLCode = false
    given Conversion[Boolean, PrintDFHDLCode] = identity

end ElaborationOptions
