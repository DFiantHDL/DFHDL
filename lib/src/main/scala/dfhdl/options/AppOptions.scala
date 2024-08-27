package dfhdl.options
import dfhdl.internals.scastieIsRunning
import AppOptions.*

case class AppOptions(
    defaultMode: DefaultMode
)

object AppOptions:
  given default(using
      defaultMode: DefaultMode
  ): AppOptions = AppOptions(defaultMode = defaultMode)

  enum DefaultMode derives CanEqual:
    case help, elaborate, compile, commit, lint
  object DefaultMode:
    given DefaultMode = if (scastieIsRunning) compile else help
