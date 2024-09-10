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
    case (DFSInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
      assert(tWidth == fWidth + 1)
      fromData
    // Bits resize
    case (DFBits(Int(tWidth)), DFBits(_)) =>
      import dfhdl.internals.{resize => resizeBV}
      val data = fromData.asInstanceOf[(BitVector, BitVector)]
      (data._1.resizeBV(tWidth), data._2.resizeBV(tWidth))
    // UInt resize
    case (DFUInt(Int(tWidth)), DFUInt(Int(fWidth))) =>
      if (tWidth > fWidth) fromData
      else
        fromData.asInstanceOf[Option[BigInt]].map(_.truncateAsUnsigned(tWidth))
    // SInt resize or conversion to/from DFInt32
    case (DFXInt(true, Int(tWidth), _), DFXInt(true, Int(fWidth), _)) =>
      if (tWidth > fWidth) fromData
      else
        fromData.asInstanceOf[Option[BigInt]].map(_.truncateAsUnsigned(tWidth).asSigned(tWidth))
    // Casting from UInt to DFInt32
    case (DFInt32, DFUInt(Int(fWidth))) =>
      assert(fWidth <= 31)
      fromData
    // Casting from any data to Bits
    case (DFBits(Int(tWidth)), _) =>
      assert(tWidth == fromType.width)
      fromType.dataToBitsData(fromData)
    // Casting from Bits to any data
    case (_, DFBits(Int(fWidth))) =>
      assert(fWidth == toType.width)
      toType.bitsDataToData(fromData.asInstanceOf[(BitVector, BitVector)])
    // Casting from any data to any data
    case _ if fromType.width == toType.width =>
      toType.bitsDataToData(fromType.dataToBitsData(fromData))
    case x =>
      println(x)
      throw new IllegalArgumentException("Unsupported data conversion")
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
  if (op == FuncOp.sel)
    val ret = (argTypes.head, argData) match
      case (DFBool | DFBit, Some(cond: Boolean) :: onTrue :: onFalse :: Nil) =>
        if (cond) onTrue else onFalse
      case x =>
        println(x)
        ???
    ret.asInstanceOf[outType.Data]
  else
    outType match
      // bits operations are handled specially, because bubble is bit-accurate
      case _: DFBits =>
        val ret: (BitVector, BitVector) = (op, argTypes, argData) match
          // bits concatenation
          case (FuncOp.++, _, argData: List[(BitVector, BitVector)] @unchecked) =>
            val (values, bubbles) = argData.unzip
            (values.bitsConcat, bubbles.bitsConcat)
          // bits repeat
          case (
                FuncOp.repeat,
                DFBits(_) :: DFInt32 :: Nil,
                (argData: (BitVector, BitVector) @unchecked) :: Some(cnt: BigInt) :: Nil
              ) =>
            val (values, bubbles) = List.fill(cnt.toInt)(argData).unzip
            (values.bitsConcat, bubbles.bitsConcat)
          // bits shifting
          case (
                op @ (FuncOp.<< | FuncOp.>>),
                DFBits(_) :: DFInt32 :: Nil,
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
                DFBits(_) :: DFBits(_) :: maybeMoreTypes,
                argData: List[(BitVector, BitVector)] @unchecked
              ) =>
            val (values, bubbles) = argData.unzip
            // bubble bits are always or-ed
            val outBubbles = bubbles.reduce(_ | _)
            op match
              case FuncOp.& => (values.reduce(_ & _), outBubbles)
              case FuncOp.| => (values.reduce(_ | _), outBubbles)
              case FuncOp.^ => (values.reduce(_ ^ _), outBubbles)
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
      case _: DFVector =>
        op match
          case FuncOp.++ => argData.toVector.asInstanceOf[outType.Data]
          case FuncOp.repeat =>
            val Some(cnt: BigInt) = argData(1): @unchecked
            Vector.fill(cnt.toInt)(argData.head).asInstanceOf[outType.Data]
          case x =>
            println(x)
            ???
      case _: DFStruct =>
        op match
          case FuncOp.++ => argData.asInstanceOf[outType.Data]
          case x =>
            println(x)
            ???
      // the rest bubble args cause a bubble output
      case _
          if argTypes.lazyZip(argData).exists((t, d) => t.isDataBubble(d.asInstanceOf[t.Data])) =>
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
                outType @ DFXInt(_, _, _),
                op @ (FuncOp.+ | FuncOp.- | FuncOp.`*` | FuncOp./ | FuncOp.% | FuncOp.<< |
                FuncOp.>> | FuncOp.** | FuncOp.max | FuncOp.min),
                DFXInt(_, _, _) :: DFXInt(_, _, _) :: maybeMoreTypes,
                argData: List[Option[BigInt]] @unchecked
              ) =>
            val lhs = argData(0).get
            val rhs = argData(1).get
            val dataNoTrunc = op match
              case FuncOp.+   => argData.map(_.get).reduce(_ + _)
              case FuncOp.-   => argData.map(_.get).reduce(_ - _)
              case FuncOp.*   => argData.map(_.get).reduce(_ * _)
              case FuncOp./   => lhs / rhs
              case FuncOp.%   => lhs % rhs
              case FuncOp.<<  => lhs << rhs.toInt
              case FuncOp.>>  => lhs >> rhs.toInt
              case FuncOp.**  => lhs pow rhs.toInt
              case FuncOp.max => argData.map(_.get).reduce(_ max _)
              case FuncOp.min => argData.map(_.get).reduce(_ min _)
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
          // bits reduction operations
          case (
                DFBit,
                op @ (FuncOp.^ | FuncOp.& | FuncOp.|),
                DFBits(_) :: Nil,
                (valueBits: BitVector, _: BitVector) :: Nil
              ) =>
            // bubble bits are always or-ed
            val data = op match
              case FuncOp.& => valueBits.toIndexedSeq.forall(identity)
              case FuncOp.| => valueBits.toIndexedSeq.exists(identity)
              case FuncOp.^ => valueBits.populationCount % 2L != 0
            Some(data)
          // Arithmetic Negation
          case (
                _: DFDecimal,
                FuncOp.unary_!,
                (_: DFDecimal) :: Nil,
                Some(data: BigInt) :: Nil
              ) =>
            Some(-data)
          // Arithmetic CLog2
          case (
                DFXInt(_, _, _),
                FuncOp.clog2,
                DFXInt(_, _, _) :: Nil,
                Some(data: BigInt) :: Nil
              ) if data.isValidInt =>
            Some(BigInt(clog2(data.toInt)))
          // DFXInt comparisons
          case (
                DFBool,
                op @ (FuncOp.< | FuncOp.> | FuncOp.<= | FuncOp.>=),
                DFXInt(_, _, _) :: DFXInt(_, _, _) :: Nil,
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
                (DFBool | DFBit) :: (DFBool | DFBit) :: maybeMoreTypes,
                argData: List[Option[Boolean]] @unchecked
              ) =>
            val data = op match
              case FuncOp.& => argData.map(_.get).reduce(_ && _)
              case FuncOp.| => argData.map(_.get).reduce(_ || _)
              case FuncOp.^ => argData.map(_.get).reduce(_ ^ _)
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
