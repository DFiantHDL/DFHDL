package dfhdl.options
import dfhdl.core.{ClkCfg, RstCfg}
import dfhdl.compiler.ir.RTDomainCfg
import ElaborationOptions.*
final case class ElaborationOptions(
    logLevel: LogLevel,
    onError: OnError,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg,
    printDesignCodeAfter: PrintDesignCodeAfter
):
  private[dfhdl] val defaultRTDomainCfg: RTDomainCfg.Explicit =
    RTDomainCfg.Explicit("main", defaultClkCfg, defaultRstCfg)
object ElaborationOptions:
  given default(using
      logLevel: LogLevel,
      onError: OnError,
      defaultClkCfg: DefaultClkCfg,
      defaultRstCfg: DefaultRstCfg,
      printDesignCodeAfter: PrintDesignCodeAfter
  ): ElaborationOptions =
    ElaborationOptions(
      logLevel = logLevel, onError = onError, defaultClkCfg = defaultClkCfg,
      defaultRstCfg = defaultRstCfg, printDesignCodeAfter = printDesignCodeAfter
    )

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

  opaque type DefaultClkCfg <: ClkCfg = ClkCfg
  object DefaultClkCfg:
    given DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
    given Conversion[ClkCfg, DefaultClkCfg] = identity
    given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]
    export ClkCfg.*

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  object DefaultRstCfg:
    given DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
    given Conversion[RstCfg, DefaultRstCfg] = identity
    given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]
    export RstCfg.*

  opaque type PrintDesignCodeAfter <: Boolean = Boolean
  object PrintDesignCodeAfter:
    given PrintDesignCodeAfter = false
    given Conversion[Boolean, PrintDesignCodeAfter] = identity

end ElaborationOptions
