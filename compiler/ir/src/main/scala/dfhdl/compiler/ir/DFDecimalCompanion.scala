package dfhdl.compiler.ir
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
abstract class DFDecimalCompanion extends DFType.Companion[DFDecimal, Option[BigInt]]

abstract class DFXIntCompanion:
  object Ops:
    def arithOp(
        dfType: DFDecimal,
        op: FuncOp,
        lhs: DFDecimal.Token,
        rhs: DFDecimal.Token
    ): DFDecimal.Token =
      val dataOut = (lhs.data, rhs.data) match
        case (Some(l), Some(r)) =>
          val dataNoTrunc = op match
            case FuncOp.+ => l + r
            case FuncOp.- => l - r
            case FuncOp.* => l * r
            case FuncOp./ => l / r
            case FuncOp.% => l % r
            case _        => ???
          val widthNoTrunc = dataNoTrunc.bitsWidth(dfType.signed)
          val dataTrunc =
            if (widthNoTrunc > dfType.width)
              dataNoTrunc.toBitVector(dfType.width).toBigInt(dfType.signed)
            else dataNoTrunc
          val dataFixSign =
            if (dataTrunc < 0 && !dfType.signed)
              dataTrunc.asUnsigned(dfType.width)
            else dataTrunc
          Some(dataFixSign)
        case _ => None
      DFXInt.Token(dfType.signed, dfType.width, dataOut)
    end arithOp
    extension (lhs: DFDecimal.Token)
      def resize(updatedWidth: Int): DFDecimal.Token =
        // no change in width
        if (updatedWidth == lhs.width) lhs
        else
          val signed = lhs.dfType.signed
          assert(signed && updatedWidth >= 1 || !signed && updatedWidth >= 0)
          // updated width is larger or the data is bubble
          if (updatedWidth > lhs.width || lhs.isBubble) DFXInt.Token(signed, updatedWidth, lhs.data)
          else // updated width is smaller
            import DFBits.Ops.{msbit, sint, uint, sel, ++}
            import DFBits.Ops.resize as resizeDFBits
            if (signed)
              val tokenBits = lhs.bits
              (tokenBits.msbit.bits ++ tokenBits.sel(updatedWidth - 2, 0)).sint
            else // unsigned
              lhs.bits.resizeDFBits(updatedWidth).uint
            end if
          end if
    end extension
  end Ops
end DFXIntCompanion

abstract class DFSIntCompanion:
  object Ops:
    extension (lhs: DFDecimal.Token)
      def signed: DFDecimal.Token =
        import DFXInt.Ops.resize
        import DFBits.Ops.sint
        lhs.resize(lhs.width + 1).bits.sint
