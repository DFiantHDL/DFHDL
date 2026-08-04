package dfhdl.core
import dfhdl.compiler.ir

sealed trait Bubble
object Bubble extends Bubble:
  enum Behaviour derives CanEqual:
    case Stall, DontCare
  given Behaviour = Behaviour.Stall
  def constValOf[T <: DFTypeAny](dfType: T, named: Boolean)(using dfc: DFC): DFConstOf[T] =
    import dfc.getSet
    // A constant member must stay concrete (its data is width-bound), and a bubble is only
    // literally representable in Bits, so a parametric-width bubble is built as a single-bit
    // bubble constant repeated by the width parameter (mirroring the parametric handling of
    // `all(bit)`), cast to the original type when that type is not Bits.
    def bitsBubbleRepeat(widthParamRef: ir.IntParamRef)(using DFC) =
      val singleBitType = DFBits.forced[1](1)
      val singleBit =
        DFVal.Const.forced(singleBitType, singleBitType.asIR.createBubbleData, named = false)
      import DFBits.Val.Ops.repeat
      singleBit.repeat(widthParamRef.get)
    val dfcArg = if (named) dfc else dfc.anonymize
    dfType.asIR match
      case ir.DFBits(widthParamRef) if !widthParamRef.isInt =>
        bitsBubbleRepeat(widthParamRef)(using dfcArg).asConstOf[T]
      case ir.DFXInt(signed, widthParamRef, _) if !widthParamRef.isInt =>
        import DFBits.Val.Ops.{uint, sint}
        val bits = bitsBubbleRepeat(widthParamRef)(using dfc.anonymize)
        if (signed) bits.sint(using dfcArg).asConstOf[T]
        else bits.uint(using dfcArg).asConstOf[T]
      case _ =>
        DFVal.Const.forced(dfType, dfType.asIR.createBubbleData, named)
    end match
  end constValOf
end Bubble

final val ? = Bubble
