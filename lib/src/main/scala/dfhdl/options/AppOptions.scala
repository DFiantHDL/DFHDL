package dfhdl.options
import dfhdl.internals.{metalsIsRunning, scala_cliIsRunning}
import dfhdl.core.Design
import AppOptions.*
import dfhdl.compiler.ir.ConfigN

case class AppOptions(
    appMode: AppMode,
    clearConsole: ClearConsole,
    cacheEnable: CacheEnable
)

object AppOptions:
  opaque type Defaults[-T <: Design] <: AppOptions = AppOptions
  object Defaults:
    given (using
        appMode: AppMode,
        clearConsole: ClearConsole,
        cacheEnable: CacheEnable
    ): Defaults[Design] =
      AppOptions(appMode = appMode, clearConsole = clearConsole, cacheEnable = cacheEnable)

  opaque type AppMode <: ConfigN[dfhdl.app.AppMode] = ConfigN[dfhdl.app.AppMode]
  object AppMode:
    export dfhdl.app.AppMode.*
    given AppMode = None
    given CanEqual[dfhdl.app.AppMode, AppMode] = CanEqual.derived
    given Conversion[dfhdl.app.AppMode, AppMode] = identity

  opaque type ClearConsole <: Boolean = Boolean
  object ClearConsole:
    given ClearConsole = if (metalsIsRunning || scala_cliIsRunning) true else false
    given Conversion[Boolean, ClearConsole] = identity

  opaque type CacheEnable <: Boolean = Boolean
  object CacheEnable:
    given CacheEnable = true
    given Conversion[Boolean, CacheEnable] = identity
end AppOptions
