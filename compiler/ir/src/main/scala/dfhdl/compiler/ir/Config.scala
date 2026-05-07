package dfhdl.compiler.ir
import dfhdl.compiler.printing.{Printer, HasCodeString}
import upickle.default.*
into opaque type ConfigN[T] = T | None.type
object ConfigN:
  given [T]: Conversion[None.type, ConfigN[T]] with
    def apply(x: None.type): ConfigN[T] = x
  given [T]: Conversion[T, ConfigN[T]] with
    def apply(x: T): ConfigN[T] = x
  implicit def fromNoneToFunc[T, Comp](from: None.type): Comp => ConfigN[T] = _ => None
  // TODO: `into` is not working with the new conversion (get warnings)
  // given [T, Comp]: Conversion[None.type, Comp => ConfigN[T]] with
  //   def apply(x: None.type): Comp => ConfigN[T] = _ => None
  given [F, T](using conv: Conversion[F, T]): Conversion[F, ConfigN[T]] with
    def apply(x: F): ConfigN[T] = conv(x)
  given [T1, T2](using CanEqual[T1, T2]): CanEqual[ConfigN[T1], ConfigN[T2]] = CanEqual.derived
  given [T]: CanEqual[ConfigN[T], None.type] = CanEqual.derived
  given [T]: CanEqual[None.type, ConfigN[T]] = CanEqual.derived
  given [T]: CanEqual[ConfigN[T], T] = CanEqual.derived
  given [T]: CanEqual[T, ConfigN[T]] = CanEqual.derived
  given [L, R]: CanEqual[ConfigN[L], ConfigN[R]] = CanEqual.derived
  given [T](using ReadWriter[T]): ReadWriter[ConfigN[T]] = readwriter[ujson.Value].bimap(
    value =>
      value.runtimeChecked match
        case None                => ujson.Null
        case value: T @unchecked => writeJs(value)
    ,
    json =>
      json match
        case ujson.Null => None
        case value      => read[T](value)
  )
  extension [T](x: ConfigN[T])
    inline def get: T = x.asInstanceOf[T]
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
    def toOption: Option[T] = x match
      case None                => None
      case value: T @unchecked => Some(value)
    def toList: List[T] = x match
      case None                => Nil
      case value: T @unchecked => List(value)
    def nonEmpty: Boolean = x != None
    def isEmpty: Boolean = x == None
  end extension
  extension [T <: DFRefAny](x: ConfigN[T])
    def =~(that: ConfigN[T])(using MemberGetSet): Boolean = (x, that) match
      case (None, None)                          => true
      case (t: T @unchecked, that: T @unchecked) => t =~ that
      case _                                     => false
end ConfigN

/** Sets the policy for inclusing the clock or reset signals when they are not needed
  */
enum ClkRstInclusionPolicy extends HasCodeString derives CanEqual, ReadWriter:
  /** Don't include if not needed
    */
  case AsNeeded

  /** Always include at the top and silence with `@unused` annotation
    */
  case AlwaysAtTop

  def codeString(using Printer): String = "_." + this.toString.toLowerCase

object ClkCfg:
  enum Edge extends HasCodeString derives CanEqual, ReadWriter:
    case Rising, Falling
    def codeString(using Printer): String = "_." + this.toString.toLowerCase
end ClkCfg

object RstCfg:
  enum Mode extends HasCodeString derives CanEqual, ReadWriter:
    case Async, Sync
    def codeString(using Printer): String = "_." + this.toString.toLowerCase
  enum Active extends HasCodeString derives CanEqual, ReadWriter:
    case Low, High
    def codeString(using Printer): String = "_." + this.toString.toLowerCase
end RstCfg
