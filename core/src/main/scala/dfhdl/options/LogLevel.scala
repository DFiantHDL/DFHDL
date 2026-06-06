package dfhdl.options
import wvlet.log.LogLevel as wvletLogLevel

type LogLevel = wvlet.log.LogLevel.type => _LogLevel
private[dfhdl] into opaque type _LogLevel <: wvletLogLevel = wvletLogLevel
object _LogLevel:
  given Conversion[wvletLogLevel, _LogLevel] = x => x.asInstanceOf[_LogLevel]
  given LogLevel = _ => wvlet.log.LogLevel.WARN
