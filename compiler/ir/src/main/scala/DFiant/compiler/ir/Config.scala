package DFiant.compiler.ir

type ConfigDN[T] = T | DerivedCfg.type | None.type
case object DerivedCfg derives CanEqual

type ConfigD[T] = T | DerivedCfg.type
given ConfigDCE[L, R]: CanEqual[ConfigD[L], ConfigD[R]] = CanEqual.derived

type NameCfg = ConfigD[String]

type RstCfg = ConfigDN[RstCfg.Explicit]
object RstCfg:
  enum Mode derives CanEqual:
    case Async, Sync
  type ModeCfg = ConfigD[Mode]
  enum Active derives CanEqual:
    case Low, High
  type ActiveCfg = ConfigD[Active]

  final case class Explicit(
      name: NameCfg,
      mode: ModeCfg,
      active: ActiveCfg
  ) derives CanEqual

  def apply(
      name: NameCfg = DerivedCfg,
      mode: ModeCfg = DerivedCfg,
      active: ActiveCfg = DerivedCfg
  ): RstCfg = Explicit(name, mode, active)
end RstCfg

type ClkCfg = ConfigDN[ClkCfg.Explicit]
object ClkCfg:
  enum Edge derives CanEqual:
    case Rising, Falling
  type EdgeCfg = ConfigD[Edge]

  final case class Explicit(
      name: NameCfg,
      edge: EdgeCfg
  ) derives CanEqual

  def apply(
      name: NameCfg = DerivedCfg,
      edge: EdgeCfg = DerivedCfg
  ): ClkCfg = Explicit(name, edge)
