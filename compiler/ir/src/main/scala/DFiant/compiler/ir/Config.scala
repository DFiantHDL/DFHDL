package DFiant.compiler.ir

//The reason we use opaque types here and not just types is because scala
//looses type refinement such as in `val clkCfg = ClkCfg(...)`
opaque type ConfigDN[T] = T | DerivedCfg.type | None.type
object ConfigDN:
  given [T]: Conversion[DerivedCfg.type, ConfigDN[T]] with
    def apply(x: DerivedCfg.type): ConfigDN[T] = x
  given [T]: Conversion[None.type, ConfigDN[T]] with
    def apply(x: None.type): ConfigDN[T] = x
  given [T]: Conversion[T, ConfigDN[T]] with
    def apply(x: T): ConfigDN[T] = x
case object DerivedCfg derives CanEqual

opaque type ConfigD[T] = T | DerivedCfg.type
object ConfigD:
  given [T]: Conversion[DerivedCfg.type, ConfigD[T]] with
    def apply(x: DerivedCfg.type): ConfigD[T] = x
  given [T]: Conversion[T, ConfigD[T]] with
    def apply(x: T): ConfigD[T] = x

given ConfigDCE[L, R]: CanEqual[ConfigD[L], ConfigD[R]] = CanEqual.derived

type NameCfg = ConfigD[String]

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
      name: NameCfg = "clk",
      edge: EdgeCfg = Edge.Rising
  ): ClkCfg = Explicit(name, edge)

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
      name: NameCfg = "rst",
      mode: ModeCfg = Mode.Sync,
      active: ActiveCfg = Active.High
  ): RstCfg = Explicit(name, mode, active)
end RstCfg
