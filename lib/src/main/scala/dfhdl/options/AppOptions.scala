package dfhdl.options
import dfhdl.internals.{scastieIsRunning, metalsIsRunning, scala_cliIsRunning}
import AppOptions.*

case class AppOptions(
    defaultMode: DefaultMode,
    clearConsole: ClearConsole
)

object AppOptions:
  given default(using
      defaultMode: DefaultMode,
      clearConsole: ClearConsole
  ): AppOptions = AppOptions(defaultMode = defaultMode, clearConsole = clearConsole)

  enum DefaultMode derives CanEqual:
    case help, elaborate, compile, commit, lint
  object DefaultMode:
    given DefaultMode =
      if (scastieIsRunning) compile
      else commit

  opaque type ClearConsole <: Boolean = Boolean
  object ClearConsole:
    given ClearConsole = if (metalsIsRunning || scala_cliIsRunning) true else false
    given Conversion[Boolean, ClearConsole] = identity
end AppOptions
