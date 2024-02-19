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
      width: IntParam[W],
      sev: SameElementsVector[T],
      named: Boolean = false
  )(using DFC): DFConstOf[DFBits[W]] =
    val boolVal = sev.value match
      case b: Boolean => b
      case i: Int     => i > 0
    def constVec[W <: Int](width: Int, named: Boolean): DFConstOf[DFBits[W]] =
      DFVal.Const(
        DFBits.forced[W](width),
        (BitVector.fill(width)(boolVal), BitVector.low(width)),
        named
      )
    width match
      case width: Int => constVec[W](width, named)
      case width: DFConstOf[DFInt32] @unchecked =>
        val singleBit = constVec[1](1, named = false)
        import DFBits.Val.Ops.repeat
        val dfcArg = if (named) dfc else dfc.anonymize
        singleBit.repeat(width)(using dfcArg).asConstOf[DFBits[W]]
  end bitsValOf

  given eqBit[W <: Int, T <: BitOrBool]: CanEqual[SameElementsVector[T], DFValOf[DFBits[W]]] =
    CanEqual.derived
  given eqVec[DFT <: DFTypeAny, D <: NonEmptyTuple, T]
      : CanEqual[SameElementsVector[T], DFValOf[DFVector[DFT, D]]] =
    CanEqual.derived
end SameElementsVector
