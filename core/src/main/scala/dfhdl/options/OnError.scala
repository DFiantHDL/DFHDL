package dfhdl.options
import dfhdl.internals.getShellCommand

enum OnError derives CanEqual:
  case Exit, Exception
object OnError:
  private lazy val sbtShellOrTestIsRunning = getShellCommand match
    case Some(cmd) if cmd.endsWith("xsbt.boot.Boot") || cmd.endsWith("xsbt.boot.Boot test") => true
    case _                                                                                  => false
  given OnError = if (sbtShellOrTestIsRunning) Exception else Exit
