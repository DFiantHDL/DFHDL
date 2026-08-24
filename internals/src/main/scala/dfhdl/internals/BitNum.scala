package dfhdl.internals

type BitNum = 0 | 1

//BitNumWrapper is a wrapper for BitNum to preserve 0 or 1 values in basic operations
//The type is also used as `Bit` in the DFHDL frontend, to allow using BitNum values in DFHDL code
//and constructing DFBit DFHDL valeu types such as `Bit <> CONST`.
//TODO: implemented workaround for https://github.com/scala/scala3/issues/26550
into sealed class BitNumWrapper(val value: Int) extends AnyVal derives CanEqual:
  def unary_! : BitNumWrapper = BitNumWrapper(if value == 0 then 1 else 0)
  def unary_~ : BitNumWrapper = unary_!
  def |(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value == 1 || rhs.value == 1 then 1 else 0)
  def &(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value == 1 && rhs.value == 1 then 1 else 0)
  def ^(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value != rhs.value then 1 else 0)
  def &&(rhs: BitNumWrapper): BitNumWrapper = this & rhs
  def ||(rhs: BitNumWrapper): BitNumWrapper = this | rhs
  def ==(rhs: BitNum): Boolean = value == rhs
  def !=(rhs: BitNum): Boolean = value != rhs

object BitNumWrapper:
  def apply(value: BitNum): BitNumWrapper = new BitNumWrapper(value)
  given [T <: Int & Singleton](using T <:< BitNum): Conversion[T, BitNumWrapper] =
    x => BitNumWrapper(x.asInstanceOf[BitNum])
  given CanEqual[BitNumWrapper, BitNum] = CanEqual.derived
  // TODO: implemented workaround for https://github.com/scala/scala3/issues/26550
  implicit def toBitNum(wrapper: BitNumWrapper): BitNum = wrapper.value.asInstanceOf[BitNum]