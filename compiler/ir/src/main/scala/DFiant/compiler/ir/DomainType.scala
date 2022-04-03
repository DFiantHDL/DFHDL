package DFiant.compiler.ir

sealed trait DomainType derives CanEqual
object DomainType:
  // dataflow domain
  sealed trait DF extends DomainType
  object DF extends DF
  // register-transfer domain
  class RT(
      val clkCfg: ClkCfg,
      val rstCfg: RstCfg
  ) extends DomainType
  // event-driven domain
  class ED() extends DomainType
end DomainType
