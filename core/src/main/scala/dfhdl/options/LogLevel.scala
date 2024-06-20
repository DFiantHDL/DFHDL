package dfhdl.options
import wvlet.log.LogLevel as wvletLogLevel

opaque type LogLevel <: wvletLogLevel = wvletLogLevel
given Conversion[wvletLogLevel, LogLevel] = x => x.asInstanceOf[LogLevel]
object LogLevel:
  export wvletLogLevel.{OFF, ERROR, WARN, INFO, DEBUG, TRACE, ALL}
  given LogLevel = wvletLogLevel.WARN
