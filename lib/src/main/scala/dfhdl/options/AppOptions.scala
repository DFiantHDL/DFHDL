package dfhdl.options
import dfhdl.internals.{scastieIsRunning, metalsIsRunning, scala_cliIsRunning}
import dfhdl.core.Design
import AppOptions.*

case class AppOptions(
    defaultMode: DefaultMode,
    clearConsole: ClearConsole,
    cacheEnable: CacheEnable
)

object AppOptions:
  opaque type Defaults[-T <: Design] <: AppOptions = AppOptions
  object Defaults:
    given (using
        defaultMode: DefaultMode,
        clearConsole: ClearConsole,
        cacheEnable: CacheEnable
    ): Defaults[Design] =
      AppOptions(defaultMode = defaultMode, clearConsole = clearConsole, cacheEnable = cacheEnable)

  enum DefaultMode derives CanEqual:
    case help, elaborate, compile, commit, lint, simulate, build, program
  object DefaultMode:
    given DefaultMode =
      if (scastieIsRunning) compile
      else commit

  opaque type ClearConsole <: Boolean = Boolean
  object ClearConsole:
    given ClearConsole = if (metalsIsRunning || scala_cliIsRunning) true else false
    given Conversion[Boolean, ClearConsole] = identity

  opaque type CacheEnable <: Boolean = Boolean
  object CacheEnable:
    given CacheEnable = true
    given Conversion[Boolean, CacheEnable] = identity
end AppOptions
