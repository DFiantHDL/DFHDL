package dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp
import dfhdl.internals.*

def dataConversion[TT <: DFType, FT <: DFType](toType: TT, fromType: FT)(
    fromData: fromType.Data
)(using MemberGetSet): toType.Data =
  val ret = (toType, fromType) match
    // no casting needed
    case (t, f) if t =~ f => fromData
    // unsigned to signed conversion
    case (DFSInt(tWidth), DFUInt(fWidth)) =>
      assert(tWidth == fWidth + 1)
      fromData
    // Bits resize
    case (DFBits(tWidth), DFBits(_)) =>
      import dfhdl.internals.{resize => resizeBV}
      val data = fromData.asInstanceOf[(BitVector, BitVector)]
      (data._1.resizeBV(tWidth), data._2.resizeBV(tWidth))
    // UInt resize
    case (DFUInt(tWidth), DFUInt(fWidth)) =>
      if (tWidth > fWidth) fromData
      else
        fromData.asInstanceOf[Option[BigInt]].map(_.truncateAsUnsigned(tWidth))
    // SInt resize
    case (DFSInt(tWidth), DFSInt(fWidth)) =>
      if (tWidth > fWidth) fromData
      else
        fromData.asInstanceOf[Option[BigInt]].map(_.truncateAsUnsigned(tWidth).asSigned(tWidth))
    // Casting from any data to Bits
    case (DFBits(tWidth), _) =>
      assert(tWidth == fromType.width)
      fromType.dataToBitsData(fromData)
    // Casting from Bits to any data
    case (_, DFBits(fWidth)) =>
      assert(fWidth == toType.width)
      toType.bitsDataToData(fromData.asInstanceOf[(BitVector, BitVector)])
    // Casting from any data to any data
    case _ =>
      assert(fromType.width == toType.width)
      toType.bitsDataToData(fromType.dataToBitsData(fromData))
  ret.asInstanceOf[toType.Data]
end dataConversion

def selBitRangeData(
    fromData: (BitVector, BitVector),
    relBitHigh: Int,
    relBitLow: Int
): (BitVector, BitVector) =
  assert(relBitHigh >= 0 && relBitHigh < fromData._1.length)
  assert(relBitLow >= 0 && relBitLow < fromData._1.length)
  assert(relBitHigh >= relBitLow)
  val valueBits =
    fromData._1.bits(relBitHigh.toLong, relBitLow.toLong)
  val bubbleBits =
    fromData._2.bits(relBitHigh.toLong, relBitLow.toLong)
  (valueBits, bubbleBits)

def calcFuncData[OT <: DFType](
    outType: OT,
    op: FuncOp,
    argTypes: List[DFType],
    argData: List[Any]
)(using MemberGetSet): outType.Data =
  outType match
    // bits operations are handled specially, because bubble is bit-accurate
    case _: DFBits =>
      val ret: (BitVector, BitVector) = (op, argTypes, argData) match
        // bits concatenation
        case (FuncOp.++, _, argData: List[(BitVector, BitVector)] @unchecked) =>
          val (values, bubbles) = argData.unzip
          (values.bitsConcat, bubbles.bitsConcat)
        // bits shifting
        case (
              op @ (FuncOp.<< | FuncOp.>>),
              DFBits(_) :: DFUInt(_) :: Nil,
              (vec: (BitVector, BitVector) @unchecked) :: Some(shift: BigInt) :: Nil
            ) =>
          op match
            case FuncOp.<< => (vec._1.shiftLeft(shift.toLong), vec._2.shiftLeft(shift.toLong))
            case FuncOp.>> =>
              (
                vec._1.shiftRight(shift.toLong, signExtension = false),
                vec._2.shiftRight(shift.toLong, signExtension = false)
              )
        // bits logic operations
        case (
              op @ (FuncOp.^ | FuncOp.& | FuncOp.|),
              DFBits(_) :: DFBits(_) :: Nil,
              (lhs: (BitVector, BitVector) @unchecked) :: (rhs: (BitVector, BitVector) @unchecked)
              :: Nil
            ) =>
          // bubble bits are always or-ed
          op match
            case FuncOp.^ => (lhs._1 ^ rhs._1, lhs._2 | rhs._2)
            case FuncOp.& => (lhs._1 & rhs._1, lhs._2 | rhs._2)
            case FuncOp.| => (lhs._1 | rhs._1, lhs._2 | rhs._2)
        case (
              FuncOp.unary_~,
              DFBits(_) :: Nil,
              (vec: (BitVector, BitVector) @unchecked) :: Nil
            ) =>
          (vec._1.not, vec._2)
        case x =>
          println(x)
          ???
      ret.asInstanceOf[outType.Data]

    // the rest bubble args cause a bubble output
    case _ if argTypes.lazyZip(argData).exists((t, d) => t.isDataBubble(d.asInstanceOf[t.Data])) =>
      outType.createBubbleData
    // the rest of the regular operations
    case _ =>
      val ret = (outType, op, argTypes, argData) match
        // equality/inequality
        case (DFBool, op @ (FuncOp.=== | FuncOp.=!=), _, lhs :: rhs :: Nil) =>
          val equals = lhs equals rhs
          Some(if (op == FuncOp.===) equals else !equals)
        // DFXInt arithmetic operations and shifting
        case (
              outType @ DFXInt(_, _),
              op @ (FuncOp.+ | FuncOp.- | FuncOp.`*` | FuncOp./ | FuncOp.% | FuncOp.<< | FuncOp.>>),
              DFXInt(_, _) :: DFXInt(_, _) :: Nil,
              Some(lhs: BigInt) :: Some(rhs: BigInt) :: Nil
            ) =>
          val dataNoTrunc = op match
            case FuncOp.+  => lhs + rhs
            case FuncOp.-  => lhs - rhs
            case FuncOp.*  => lhs * rhs
            case FuncOp./  => lhs / rhs
            case FuncOp.%  => lhs % rhs
            case FuncOp.<< => lhs << rhs.toInt
            case FuncOp.>> => lhs >> rhs.toInt
          val widthNoTrunc = dataNoTrunc.bitsWidth(outType.signed)
          val dataTrunc =
            if (widthNoTrunc > outType.width)
              dataNoTrunc.toBitVector(outType.width).toBigInt(outType.signed)
            else dataNoTrunc
          val dataFixSign =
            if (dataTrunc < 0 && !outType.signed)
              dataTrunc.asUnsigned(outType.width)
            else dataTrunc
          Some(dataFixSign)
        // Arithmetic Negation
        case (
              _: DFDecimal,
              FuncOp.unary_!,
              (_: DFDecimal) :: Nil,
              Some(data: BigInt) :: Nil
            ) =>
          Some(-data)
        // DFXInt comparisons
        case (
              DFBool,
              op @ (FuncOp.< | FuncOp.> | FuncOp.<= | FuncOp.>=),
              DFXInt(_, _) :: DFXInt(_, _) :: Nil,
              Some(lhs: BigInt) :: Some(rhs: BigInt) :: Nil
            ) =>
          val data = op match
            case FuncOp.<  => lhs < rhs
            case FuncOp.>  => lhs > rhs
            case FuncOp.<= => lhs <= rhs
            case FuncOp.>= => lhs >= rhs
          Some(data)
        // Boolean/Bit logic operations
        case (
              DFBool | DFBit,
              op @ (FuncOp.^ | FuncOp.& | FuncOp.|),
              (DFBool | DFBit) :: (DFBool | DFBit) :: Nil,
              Some(lhs: Boolean) :: Some(rhs: Boolean) :: Nil
            ) =>
          val data = op match
            case FuncOp.^ => lhs ^ rhs
            case FuncOp.& => lhs & rhs
            case FuncOp.| => lhs | rhs
          Some(data)
        // Boolean/Bit logical invert
        case (
              DFBool | DFBit,
              FuncOp.unary_!,
              (DFBool | DFBit) :: Nil,
              Some(data: Boolean) :: Nil
            ) =>
          Some(!data)
        case x =>
          println(x)
          ???
      ret.asInstanceOf[outType.Data]
end calcFuncData
