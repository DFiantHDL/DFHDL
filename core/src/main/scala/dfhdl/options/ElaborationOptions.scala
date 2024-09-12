package dfhdl.options
import dfhdl.core.{ClkCfg, RstCfg}
import dfhdl.compiler.ir.RTDomainCfg
import ElaborationOptions.*
final case class ElaborationOptions(
    logLevel: LogLevel,
    onError: OnError,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg,
    printDFHDLCode: PrintDFHDLCode
):
  private[dfhdl] val defaultRTDomainCfg: RTDomainCfg.Explicit =
    RTDomainCfg.Explicit("RTDomainCfg.Default", defaultClkCfg, defaultRstCfg)
object ElaborationOptions:
  given default(using
      logLevel: LogLevel,
      onError: OnError,
      defaultClkCfg: DefaultClkCfg,
      defaultRstCfg: DefaultRstCfg,
      printDFHDLCode: PrintDFHDLCode
  ): ElaborationOptions =
    ElaborationOptions(
      logLevel = logLevel, onError = onError, defaultClkCfg = defaultClkCfg,
      defaultRstCfg = defaultRstCfg, printDFHDLCode = printDFHDLCode
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
    given DefaultClkCfg = ClkCfg()
    given Conversion[ClkCfg, DefaultClkCfg] = identity
    given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]
    export ClkCfg.*

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  object DefaultRstCfg:
    given DefaultRstCfg = RstCfg()
    given Conversion[RstCfg, DefaultRstCfg] = identity
    given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]
    export RstCfg.*

  opaque type PrintDFHDLCode <: Boolean = Boolean
  object PrintDFHDLCode:
    given PrintDFHDLCode = false
    given Conversion[Boolean, PrintDFHDLCode] = identity

end ElaborationOptions
