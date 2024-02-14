package dfhdl.core

import dfhdl.internals.*
import scala.annotation.targetName

/** A unconstrained-width literal vector of a given bit value
  *
  * @example
  *   {{{
  *   val x = DFBits(8) init all(0)
  *   x := all(1)
  *   }}}
  * @note
  *   Some vector operations are not possible with this literal. E.g., `x ++ all(0)` is forbidden
  *   because concatenation cannot infer the output width from this operation.
  */
final class SameElementsVector[T](val value: T) derives CanEqual:
  override def equals(that: Any): Boolean = that match
    case sev: SameElementsVector[?] => value equals sev.value
    case _                          => false
  override def hashCode(): Int = value.hashCode()
end SameElementsVector

object SameElementsVector:
  def apply[T](exact: Inlined[T]): SameElementsVector[T] =
    new SameElementsVector[T](exact)
  // hacked unapply will be replaced by plugin
  def unapply[T, R](arg: SameElementsVector[T]): Option[R] = Some(
    arg.value.asInstanceOf[R]
  )
  protected[core] def bitsValOf[W <: Int, T <: BitOrBool](
      width: Inlined[W],
      sev: SameElementsVector[T],
      named: Boolean = false
  )(using DFC): DFConstOf[DFBits[W]] =
    val boolVal = sev.value match
      case b: Boolean => b
      case i: Int     => i > 0
    DFVal.Const(
      DFBits.fromInlined(width),
      (BitVector.fill(width.value)(boolVal), BitVector.low(width.value)),
      named
    )
  end bitsValOf

  given eqBit[W <: Int, T <: BitOrBool]: CanEqual[SameElementsVector[T], DFValOf[DFBits[W]]] =
    CanEqual.derived
  given eqVec[DFT <: DFTypeAny, D <: NonEmptyTuple, T]
      : CanEqual[SameElementsVector[T], DFValOf[DFVector[DFT, D]]] =
    CanEqual.derived
end SameElementsVector
