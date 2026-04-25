package dfhdl.options
import dfhdl.core.{Design, DFConstOf, DFTime, DFFreq}
import dfhdl.compiler.ir
import dfhdl.compiler.ir.{
  ClkCfg, RstCfg, ClkRstInclusionPolicy, RateNumber, DefaultRTDomainCfgTag, constraints
}
import dfhdl.core.DFPhysical.Val.Ops.MHz
import ElaborationOptions.*
final case class ElaborationOptions(
    logLevel: LogLevel,
    onError: OnError,
    Werror: WError,
    trapErrors: TrapErrors,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg,
    printDFHDLCode: PrintDFHDLCode
):
  private[dfhdl] val defaultRTDomainCfgTag: DefaultRTDomainCfgTag =
    DefaultRTDomainCfgTag(defaultClkCfg.asIR, defaultRstCfg.asIR)
end ElaborationOptions
object ElaborationOptions:
  into opaque type Defaults[-T] <: ElaborationOptions = ElaborationOptions
  object Defaults:
    given conv[T]: Conversion[ElaborationOptions, Defaults[T]] = identity
    given (using
        logLevel: LogLevel,
        onError: OnError,
        Werror: WError,
        trapErrors: TrapErrors,
        defaultClkCfg: DefaultClkCfg,
        defaultRstCfg: DefaultRstCfg,
        printDFHDLCode: PrintDFHDLCode
    ): Defaults[Any] = ElaborationOptions(
      logLevel = logLevel, onError = onError, Werror = Werror, trapErrors = trapErrors,
      defaultClkCfg = defaultClkCfg, defaultRstCfg = defaultRstCfg, printDFHDLCode = printDFHDLCode
    )
  end Defaults
  given defaults(using defaults: Defaults[Design]): ElaborationOptions = defaults

  into opaque type LogLevel <: dfhdl.options.LogLevel = dfhdl.options.LogLevel
  object LogLevel:
    given Conversion[wvlet.log.LogLevel, LogLevel] = x => x.asInstanceOf[LogLevel]
    given (using logLevel: dfhdl.options.LogLevel): LogLevel = logLevel
    export dfhdl.options.LogLevel.*

  into opaque type OnError <: dfhdl.options.OnError = dfhdl.options.OnError
  object OnError:
    given (using onError: dfhdl.options.OnError): OnError = onError
    given Conversion[dfhdl.options.OnError, OnError] = x => x.asInstanceOf[OnError]
    export dfhdl.options.OnError.*

  into opaque type WError <: dfhdl.options.WError = dfhdl.options.WError
  object WError:
    given (using werror: dfhdl.options.WError): WError = werror
    given [T](using conv: Conversion[T, dfhdl.options.WError]): Conversion[T, WError] = t =>
      conv(t).asInstanceOf[WError]
    given Conversion[dfhdl.options.WError, WError] = identity

  into opaque type TrapErrors <: Boolean = Boolean
  object TrapErrors:
    given TrapErrors = true
    given Conversion[Boolean, TrapErrors] = identity

  into opaque type DefaultClkCfg <: constraints.Timing.Clock = constraints.Timing.Clock
  object DefaultClkCfg:
    extension (cfg: DefaultClkCfg) def asIR: constraints.Timing.Clock = cfg
    given default(using
        edge: Edge,
        rate: Rate,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): DefaultClkCfg = constraints.Timing.Clock(
      rate = rate,
      edge = edge,
      portName = portName,
      inclusionPolicy = inclusionPolicy,
      grpName = "default"
    )
    given Conversion[constraints.Timing.Clock, DefaultClkCfg] = identity
    given conversionFromTimingClock(using
        edge: Edge,
        rate: Rate,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): Conversion[dfhdl.hw.constraints.timing.clock, DefaultClkCfg] = userClk =>
      val ir = userClk.asIR
      constraints.Timing.Clock(
        rate = ir.rate.getOrElse(rate),
        edge = ir.edge.getOrElse(edge),
        portName = ir.portName.getOrElse(portName),
        inclusionPolicy = ir.inclusionPolicy.getOrElse(inclusionPolicy),
        grpName = ir.grpName.getOrElse("default"),
        bitIdx = ir.bitIdx
      )
    given Conversion[None.type, DefaultClkCfg] =
      _ => constraints.Timing.Clock(grpName = "default")
    into opaque type Edge <: ClkCfg.Edge = ClkCfg.Edge
    object Edge:
      given Edge = Edge.Rising
      given Conversion[ClkCfg.Edge, Edge] = identity
      export ClkCfg.Edge.*
    into opaque type Rate <: RateNumber = RateNumber
    object Rate:
      given Rate = (50.MHz: RateNumber)
      given Conversion[RateNumber, Rate] = identity
      given Conversion[DFConstOf[DFTime | DFFreq], Rate] = x => (x: RateNumber)
    into opaque type PortName <: String = String
    object PortName:
      given PortName = "clk"
      given Conversion[String, PortName] = identity
    into opaque type InclusionPolicy <: ClkRstInclusionPolicy = ClkRstInclusionPolicy
    object InclusionPolicy:
      given InclusionPolicy = InclusionPolicy.AsNeeded
      given Conversion[ClkRstInclusionPolicy, InclusionPolicy] = identity
      export ClkRstInclusionPolicy.*
  end DefaultClkCfg

  into opaque type DefaultRstCfg <: constraints.Timing.Reset = constraints.Timing.Reset
  object DefaultRstCfg:
    extension (cfg: DefaultRstCfg) def asIR: constraints.Timing.Reset = cfg
    given default(using
        mode: Mode,
        active: Active,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): DefaultRstCfg = constraints.Timing.Reset(
      mode = mode,
      active = active,
      portName = portName,
      inclusionPolicy = inclusionPolicy
    )
    given Conversion[constraints.Timing.Reset, DefaultRstCfg] = identity
    given conversionFromTimingReset(using
        mode: Mode,
        active: Active,
        portName: PortName,
        inclusionPolicy: InclusionPolicy
    ): Conversion[dfhdl.hw.constraints.timing.reset, DefaultRstCfg] = userRst =>
      val ir = userRst.asIR
      constraints.Timing.Reset(
        mode = ir.mode.getOrElse(mode),
        active = ir.active.getOrElse(active),
        portName = ir.portName.getOrElse(portName),
        inclusionPolicy = ir.inclusionPolicy.getOrElse(inclusionPolicy),
        bitIdx = ir.bitIdx
      )
    given Conversion[None.type, DefaultRstCfg] = _ => constraints.Timing.Reset()
    into opaque type Mode <: RstCfg.Mode = RstCfg.Mode
    object Mode:
      given Mode = Mode.Sync
      given Conversion[RstCfg.Mode, Mode] = identity
      export RstCfg.Mode.*
    into opaque type Active <: RstCfg.Active = RstCfg.Active
    object Active:
      given Active = Active.High
      given Conversion[RstCfg.Active, Active] = identity
      export RstCfg.Active.*
    into opaque type PortName <: String = String
    object PortName:
      given PortName = "rst"
      given Conversion[String, PortName] = identity
    into opaque type InclusionPolicy <: ClkRstInclusionPolicy = ClkRstInclusionPolicy
    object InclusionPolicy:
      given InclusionPolicy = InclusionPolicy.AsNeeded
      given Conversion[ClkRstInclusionPolicy, InclusionPolicy] = identity
      export ClkRstInclusionPolicy.*
  end DefaultRstCfg
  into opaque type PrintDFHDLCode <: Boolean = Boolean
  object PrintDFHDLCode:
    given PrintDFHDLCode = false
    given Conversion[Boolean, PrintDFHDLCode] = identity

end ElaborationOptions
