package DFiant.compiler.ir

sealed trait DomainType derives CanEqual
object DomainType:
  // dataflow domain
  sealed trait DF extends DomainType
  object DF extends DF
  // register-transfer domain
  class RT(
      val clkParams: RT.ClockParams = RT.ClockParams(),
      val rstParams: RT.ResetParams = RT.ResetParams()
  ) extends DomainType
  object RT:
    sealed trait ResetParams extends Product with Serializable derives CanEqual
    case object NoReset extends ResetParams
    final case class WithReset(
        name: String,
        mode: ResetParams.Mode,
        active: ResetParams.Active
    ) extends ResetParams
    object ResetParams:
      def apply(
          name: String = "rst",
          mode: Mode = Mode.Async,
          active: Active = Active.High
      ): ResetParams = WithReset(name, mode, active)
      enum Mode derives CanEqual:
        case Async, Sync
      enum Active derives CanEqual:
        case Low, High
    end ResetParams

    sealed trait ClockParams extends Product with Serializable derives CanEqual
    case object NoClock extends ClockParams
    final case class WithClock(
        name: String,
        edge: ClockParams.Edge
    ) extends ClockParams
    object ClockParams:
      def apply(
          name: String = "clk",
          edge: ClockParams.Edge = Edge.Rising
      ): ClockParams = WithClock(name, edge)
      enum Edge derives CanEqual:
        case Rising, Falling
  end RT
  // event-driven domain
  class ED() extends DomainType
end DomainType
