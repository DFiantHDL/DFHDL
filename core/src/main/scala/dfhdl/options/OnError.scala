package dfhdl.options
import dfhdl.internals.{sbtShellIsRunning, sbtTestIsRunning, sbtnIsRunning}

type OnError = _OnError.type => _OnError
val OnError = _OnError
protected[dfhdl] enum _OnError derives CanEqual:
  case Exit, Exception
protected[dfhdl] object _OnError:
  given OnError =
    _ => if (sbtShellIsRunning || sbtnIsRunning || sbtTestIsRunning) Exception else Exit
