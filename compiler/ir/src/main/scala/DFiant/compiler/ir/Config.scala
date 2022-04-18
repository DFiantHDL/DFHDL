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
  given [T1, T2](using CanEqual[T1, T2]): CanEqual[ConfigDN[T1], ConfigDN[T2]] = CanEqual.derived
  given [T]: CanEqual[ConfigDN[T], DerivedCfg.type] = CanEqual.derived
  given [T]: CanEqual[ConfigDN[T], None.type] = CanEqual.derived
case object DerivedCfg derives CanEqual

opaque type ConfigD[T] = T | DerivedCfg.type
object ConfigD:
  given [T]: Conversion[DerivedCfg.type, ConfigD[T]] with
    def apply(x: DerivedCfg.type): ConfigD[T] = x
  given [T]: Conversion[T, ConfigD[T]] with
    def apply(x: T): ConfigD[T] = x
  given [T1, T2](using CanEqual[T1, T2]): CanEqual[ConfigD[T1], ConfigD[T2]] = CanEqual.derived
  given [T]: CanEqual[ConfigD[T], DerivedCfg.type] = CanEqual.derived
given ConfigDCE[L, R]: CanEqual[ConfigD[L], ConfigD[R]] = CanEqual.derived

type NameCfg = ConfigD[String]

type ClkCfg = ConfigDN[ClkCfg.Explicit]
object ClkCfg:
  enum Edge derives CanEqual:
    case Rising, Falling
  type EdgeCfg = ConfigD[Edge]

  final case class Explicit(
      edge: EdgeCfg
  ) derives CanEqual

type RstCfg = ConfigDN[RstCfg.Explicit]
object RstCfg:
  enum Mode derives CanEqual:
    case Async, Sync
  type ModeCfg = ConfigD[Mode]
  enum Active derives CanEqual:
    case Low, High
  type ActiveCfg = ConfigD[Active]

  final case class Explicit(
      mode: ModeCfg,
      active: ActiveCfg
  ) derives CanEqual
end RstCfg

type RTDomainCfg = ConfigD[RTDomainCfg.Explicit]
object RTDomainCfg:
  final case class Explicit(name: String, clkCfg: ClkCfg, rstCfg: RstCfg) extends NamedGlobal
      derives CanEqual
