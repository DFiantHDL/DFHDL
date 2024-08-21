package dfhdl.options
import dfhdl.internals.{sbtShellIsRunning, sbtTestIsRunning}

enum OnError derives CanEqual:
  case Exit, Exception
object OnError:
  given OnError = if (sbtShellIsRunning || sbtTestIsRunning) Exception else Exit
