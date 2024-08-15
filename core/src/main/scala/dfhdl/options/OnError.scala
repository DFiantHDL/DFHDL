package dfhdl.options
import dfhdl.internals.getShellCommand

enum OnError derives CanEqual:
  case Exit, Exception
object OnError:
  private val possibleCommandsSuffix = List(
    "xsbt.boot.Boot",
    "xsbt.boot.Boot test",
    "sbt-launch.jar",
    "sbt-launch.jar test"
  )
  private lazy val sbtShellOrTestIsRunning = getShellCommand match
    case Some(cmd) if possibleCommandsSuffix.exists(suf => cmd.endsWith(suf)) => true
    case _                                                                    => false
  given OnError = if (sbtShellOrTestIsRunning) Exception else Exit
