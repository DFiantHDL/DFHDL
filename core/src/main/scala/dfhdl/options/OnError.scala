package dfhdl.options
import dfhdl.internals.getShellCommand

enum OnError derives CanEqual:
  case Exit, Exception
object OnError:
  private lazy val sbtShellIsRunning = getShellCommand match
    case Some(cmd) if cmd.endsWith("xsbt.boot.Boot") => true
    case _                                           => false
  given OnError = if (sbtShellIsRunning) Exception else Exit
