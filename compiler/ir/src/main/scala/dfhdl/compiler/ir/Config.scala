package dfhdl.compiler.ir
import dfhdl.internals.StableEnum
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
        case value: T => writeJs(value)
    ,
    json =>
      json match
        case ujson.Null => None
        case value      => read[T](value)
  )
  extension [T](x: ConfigN[T])
    def getOrElse(default: => T): T = x match
      case None                => default
      case value: T @unchecked => value
    def foreach(f: T => Unit): Unit = x match
      case None                => ()
      case value: T @unchecked => f(value)
    def map[R](f: T => R): ConfigN[R] = x match
      case None                => None
      case value: T @unchecked => f(value)
    def flatMap[R](f: T => ConfigN[R]): ConfigN[R] = x match
      case None                => None
      case value: T @unchecked => f(value)
    def toList: List[T] = x match
      case None                => Nil
      case value: T @unchecked => List(value)
  end extension
  extension [T <: DFRefAny](x: ConfigN[T])
    def =~(that: ConfigN[T])(using MemberGetSet): Boolean = (x, that) match
      case (None, None)                          => true
      case (t: T @unchecked, that: T @unchecked) => t =~ that
      case _                                     => false
end ConfigN

/** Sets the policy for inclusing the clock or reset signals when they are not needed
  */
enum ClkRstInclusionPolicy extends StableEnum derives CanEqual, ReadWriter:
  /** Don't include if not needed
    */
  case AsNeeded

  /** Always include at the top and silence with `@unused` annotation
    */
  case AlwaysAtTop

type ClkCfg = ConfigN[ClkCfg.Explicit]
object ClkCfg:
  enum Edge extends StableEnum derives CanEqual, ReadWriter:
    case Rising, Falling

  final case class Explicit(
      edge: Edge,
      rate: RateNumber,
      portName: String,
      inclusionPolicy: ClkRstInclusionPolicy
  ) derives CanEqual,
        ReadWriter
end ClkCfg

type RstCfg = ConfigN[RstCfg.Explicit]
object RstCfg:
  enum Mode extends StableEnum derives CanEqual, ReadWriter:
    case Async, Sync
  enum Active extends StableEnum derives CanEqual, ReadWriter:
    case Low, High

  final case class Explicit(
      mode: Mode,
      active: Active,
      portName: String,
      inclusionPolicy: ClkRstInclusionPolicy
  ) derives CanEqual,
        ReadWriter
end RstCfg

enum RTDomainCfg extends HasRefCompare[RTDomainCfg], StableEnum derives CanEqual, ReadWriter:
  case Derived
  case Related(relatedDomainRef: RTDomainCfg.RelatedDomainRef) extends RTDomainCfg
  case Explicit(name: String, clkCfg: ClkCfg, rstCfg: RstCfg) extends RTDomainCfg

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
