package DFiant.internals

class GuardedRun(block : => Unit) {
  import GuardedRun.RunStatus
  private var runStatus : RunStatus = RunStatus.Idle
  def run() : Unit = runStatus match {
    case RunStatus.Idle =>
      runStatus = RunStatus.Running
      block()
    case RunStatus.Done =>
      runStatus = RunStatus.Running

  }
}
object GuardedRun {
  sealed trait RunStatus
  object RunStatus {
    case object Idle extends RunStatus
    case object Done extends RunStatus
    case object Running extends RunStatus
    case object Restart extends RunStatus
  }
}
