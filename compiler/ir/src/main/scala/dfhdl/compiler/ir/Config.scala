package dfhdl.compiler.ir
import upickle.default.*
opaque type ConfigN[T] = T | None.type
object ConfigN:
  given [T]: Conversion[None.type, ConfigN[T]] with
    def apply(x: None.type): ConfigN[T] = x
  given [T]: Conversion[T, ConfigN[T]] with
    def apply(x: T): ConfigN[T] = x
  given [T1, T2](using CanEqual[T1, T2]): CanEqual[ConfigN[T1], ConfigN[T2]] = CanEqual.derived
  given [T]: CanEqual[ConfigN[T], None.type] = CanEqual.derived
  given [T]: CanEqual[None.type, ConfigN[T]] = CanEqual.derived
  given [L, R]: CanEqual[ConfigN[L], ConfigN[R]] = CanEqual.derived
  given [T](using ReadWriter[T]): ReadWriter[ConfigN[T]] = readwriter[ujson.Value].bimap(
    value =>
      (value: @unchecked) match
        case None     => ujson.Null
        case value: T => write(value)
    ,
    json =>
      json match
        case ujson.Null => None
        case value      => read[T](value)
  )
end ConfigN

/** Sets the policy for inclusing the clock or reset signals when they are not needed
  */
enum ClkRstInclusionPolicy derives CanEqual:
  /** Don't include if not needed
    */
  case AsNeeded

  /** Always include at the top and silence with `@unused` annotation
    */
  case AlwaysAtTop

type ClkCfg = ConfigN[ClkCfg.Explicit]
object ClkCfg:
  enum Edge derives CanEqual:
    case Rising, Falling

  type RateData = (BigDecimal, DFFreq.Unit | DFTime.Unit)

  final case class Explicit(
      edge: Edge,
      rate: RateData,
      portName: String,
      inclusionPolicy: ClkRstInclusionPolicy
  ) derives CanEqual
end ClkCfg

type RstCfg = ConfigN[RstCfg.Explicit]
object RstCfg:
  enum Mode derives CanEqual:
    case Async, Sync
  enum Active derives CanEqual:
    case Low, High

  final case class Explicit(
      mode: Mode,
      active: Active,
      portName: String,
      inclusionPolicy: ClkRstInclusionPolicy
  ) derives CanEqual
end RstCfg

enum RTDomainCfg extends HasRefCompare[RTDomainCfg] derives CanEqual:
  case Derived
  case Related(relatedDomainRef: RTDomainCfg.RelatedDomainRef) extends RTDomainCfg
  case Explicit(name: String, clkCfg: ClkCfg, rstCfg: RstCfg) extends RTDomainCfg, NamedGlobal

  def isDerivedNoRst: Boolean = this match
    case cfg: Explicit if cfg.name.endsWith(".norst") => true
    case _                                            => false

  def norst: this.type = this match
    case cfg: Explicit if cfg.rstCfg != None && !cfg.isDerivedNoRst =>
      Explicit(s"${cfg.name}.norst", cfg.clkCfg, None).asInstanceOf[this.type]
    case _ => this

  protected def `prot_=~`(that: RTDomainCfg)(using MemberGetSet): Boolean =
    (this, that) match
      case (Related(thisRef), Related(thatRef)) => thisRef =~ thatRef
      case (
            Explicit(thisName, thisClkCfg: ClkCfg.Explicit, thisRstCfg),
            Explicit(thatName, thatClkCfg: ClkCfg.Explicit, thatRstCfg)
          ) =>
        thisName == thatName && thisClkCfg == thatClkCfg && thisRstCfg == thatRstCfg
      case _ => this == that

  lazy val getRefs: List[DFRef.TwoWayAny] = this match
    case Related(relatedDomainRef) => List(relatedDomainRef)
    case _                         => Nil

  def copyWithNewRefs(using RefGen): this.type = this match
    case Related(relatedDomainRef) => Related(relatedDomainRef.copyAsNewRef).asInstanceOf[this.type]
    case _                         => this
end RTDomainCfg

object RTDomainCfg:
  type RelatedDomainRef = DFRef.TwoWay[DomainBlock | DFDesignBlock, DomainBlock]
