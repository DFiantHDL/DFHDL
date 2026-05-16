package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import dfhdl.core.DFBits.{BitIndex, BitsHiLo}
import dfhdl.compiler.printing.{DefaultPrinter, Printer}

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]

object DFVector:
  protected[core] def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: List[IntParam[Int]]
  )(using DFC): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.map(_.ref))
      .asFE[DFVector[T, D]]
  given [T <: DFTypeAny, D <: IntP](using
      dfc: DFCG,
      cellType: T,
      d: ValueOf[D],
      check: VectorLength.CheckNUB[D]
  ): DFVector[T, Tuple1[D]] = trydf:
    val cellDim = IntParam.fromValue(d)
    cellDim.toScalaIntOpt.foreach(check(_))
    DFVector(cellType, List(cellDim))

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = dfType.asIR.cellType.asFE[T]
  extension [T <: DFTypeAny, D1 <: IntP](dfType: DFVector[T, Tuple1[D1]])
    def lengthIntOpt(using dfc: DFC): Option[Int] =
      import dfc.getSet
      dfType.asIR.cellDimParamRefs.head.getIntOpt
    def lengthIntParam(using dfc: DFC): IntParam[D1] =
      dfType.asIR.cellDimParamRefs.head.get.asInstanceOf[IntParam[D1]]

  protected[core] object VectorLength
      extends Check1[
        Int,
        [L <: Int] =>> L > 0,
        [L <: Int] =>> "The vector length must be positive but found: " + L
      ]

  protected[core] object IndexWidth
      extends Check2[
        Int,
        Int,
        [IW <: Int, W <: Int] =>> IW == W,
        [IW <: Int, W <: Int] =>> "The index width " + IW +
          " is different than the expected width of the vector address " + W
      ]
  protected object `LL == RL`
      extends Check2[
        Int,
        Int,
        [LL <: Int, RL <: Int] =>> LL == RL,
        [LL <: Int, RL <: Int] =>> "The argument vector length (" + RL +
          ") is different than the receiver vector length (" + LL + ")."
      ]

  object Ops:
    extension [T <: DFType.Supported, D <: IntP](t: T)(using tc: DFType.TC[T])
      infix def X(
          cellDim: IntParam[D]
      )(using
          dfc: DFCG
      )(using check: VectorLength.CheckNUB[D]): DFVector[tc.Type, Tuple1[D]] =
        trydf:
          cellDim.toScalaIntOpt.foreach(check(_))
          DFVector[tc.Type, Tuple1[D]](tc(t), List(cellDim))
  end Ops

  object Val:
    def apply[T <: DFTypeAny, D1 <: IntP, P](vectorType: DFVector[T, Tuple1[D1]])(
        from: List[DFValTP[T, P]]
    )(using dfc: DFC): DFValTP[DFVector[T, Tuple1[D1]], P] =
      import dfc.getSet
      val fromIR = from.map(_.asIR)
      lazy val commonMeta = fromIR.head.meta
      // a vector of fully anonymous constants that have the same meta information will become
      // a constant vector to save on references
      DFVal.Func(vectorType, FuncOp.++, from)
    def conv[T <: DFTypeAny, P](cellType: T, cellDim: Int)(from: Iterable[DFValTP[T, P]])(using
        DFC
    ): DFValTP[DFVector[T, Tuple1[cellDim.type]], P] =
      Val(DFVector(cellType, List(cellDim)))(from.toList)
    object TC:
      import DFVal.TC
      given DFVectorValFromDFVectorVal[
          T <: DFTypeAny,
          D1 <: IntP,
          RT <: DFTypeAny,
          RD1 <: IntP,
          RP,
          R <: DFValTP[DFVector[RT, Tuple1[RD1]], RP]
      ](using
          cellTC: TC[T, DFValOf[RT]],
          check: `LL == RL`.CheckNUB[D1, RD1]
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using dfc: DFC): Out =
          import dfc.getSet
          given Printer = DefaultPrinter
          (dfType.lengthIntOpt, arg.dfType.lengthIntOpt) match
            case (Some(ll), Some(rl)) => check(ll, rl)
            case _                    =>
              val dfTypeLengthRef = dfType.asIR.cellDimParamRefs.head
              val argLengthRef = arg.dfType.asIR.cellDimParamRefs.head
              if (dfTypeLengthRef.compare(argLengthRef)(_ != _).getOrElse(true))
                val dfTypeLengthStr = dfTypeLengthRef.refCodeString
                val argLengthStr = argLengthRef.refCodeString
                throw new IllegalArgumentException(
                  s"""The argument vector length ($argLengthStr) is different than the receiver vector length ($dfTypeLengthStr)."""
                )
          if (!dfType.asIR.isSimilarTo(arg.dfType.asIR))
            throw new IllegalArgumentException(
              s"""|Vector types must be the same when applying one vector onto another.
                  |Expected type: ${dfType.codeString}
                  |Found type:    ${arg.dfType.codeString}""".stripMargin
            )
          arg.asValTP[DFVector[T, Tuple1[D1]], RP]
        end conv
      end DFVectorValFromDFVectorVal
      given DFVectorValFromDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          RP,
          TCE <: TC[T, E]
      ](using
          tc: TCE { type OutP = RP }
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)(using dfc.anonymize)).toList
          val check = summon[`LL == RL`.Check[Int, Int]]
          dfType.lengthIntOpt match
            case Some(ll) => check(ll, dfVals.length)
            case None     =>
              val dfTypeLengthStr = dfType.asIR.cellDimParamRefs.head.refCodeString
              throw new IllegalArgumentException(
                s"""The argument vector length (${dfVals.length}) is different than the receiver vector length ($dfTypeLengthStr)."""
              )
          Val(dfType)(dfVals)
      end DFVectorValFromDFValVector
      given DFVectorValFromSEV[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: SameElementsVector[E],
          RP,
          TCE <: TC[T, E]
      ](using
          tc: TCE { type OutP = RP }
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val elem = tc.conv(dfType.cellType, arg.value)(using dfc.anonymize)
          val repeatCnt = dfType.lengthIntParam.toDFConst
          DFVal.Func(dfType, FuncOp.repeat, List(elem, repeatCnt))
            .asValTP[DFVector[T, Tuple1[D1]], OutP]
      end DFVectorValFromSEV
    end TC

    object TCConv:
      import DFVal.TCConv
      given DFVectorValFromDFValVectorConv[
          T <: DFTypeAny,
          E,
          RP,
          R <: Iterable[E],
          TCE <: TCConv[T, E]
      ](using
          tc: TCE { type OutP = RP }
      ): TCConv[DFVector[T, Tuple1[Int]], R] with
        type OutP = RP
        def apply(arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc(_)(using dfc.anonymize)).toList
          val dfType = DFVector(dfVals.head.dfType, List(dfVals.length))
          Val(dfType)(dfVals)

    end TCConv
    object Compare:
      import DFVal.Compare
      given DFVectorCompareFromDFVectorCompare[
          T <: DFTypeAny,
          D1 <: IntP,
          RT <: DFTypeAny,
          RD1 <: IntP,
          RP,
          R <: DFValTP[DFVector[RT, Tuple1[RD1]], RP],
          Op <: FuncOp,
          C <: Boolean
      ](using
          op: ValueOf[Op],
          castle: ValueOf[C],
          cellTC: Compare[T, DFValOf[RT], Op, C],
          check: `LL == RL`.CheckNUB[D1, RD1]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          import dfc.getSet
          given Printer = DefaultPrinter
          (dfType.lengthIntOpt, arg.dfType.lengthIntOpt) match
            case (Some(ll), Some(rl)) => check(ll, rl)
            case _                    =>
              val dfTypeLengthRef = dfType.asIR.cellDimParamRefs.head
              val argLengthRef = arg.dfType.asIR.cellDimParamRefs.head
              if (dfTypeLengthRef.compare(argLengthRef)(_ != _).getOrElse(true))
                val dfTypeLengthStr = dfTypeLengthRef.refCodeString
                val argLengthStr = argLengthRef.refCodeString
                throw new IllegalArgumentException(
                  s"""The argument vector length ($argLengthStr) is different than the receiver vector length ($dfTypeLengthStr)."""
                )
          if (!dfType.asIR.isSimilarTo(arg.dfType.asIR))
            throw new IllegalArgumentException(
              s"""|Vector types must be the same when comparing one vector to another.
                  |Expected type: ${dfType.codeString}
                  |Found type:    ${arg.dfType.codeString}""".stripMargin
            )
          arg.asValTP[DFVector[T, Tuple1[D1]], RP]
        end conv
      end DFVectorCompareFromDFVectorCompare

      given DFVectorCompareDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          Op <: FuncOp,
          C <: Boolean,
          RP,
          TC <: Compare[T, E, Op, C]
      ](using
          tc: TC { type OutP = RP },
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)(using dfc.anonymize)).toList
          dfType.lengthIntOpt match
            case Some(ll) =>
              val check = summon[`LL == RL`.Check[Int, Int]]
              check(ll, dfVals.length)
            case None =>
              val dfTypeLengthStr = dfType.asIR.cellDimParamRefs.head.refCodeString
              throw new IllegalArgumentException(
                s"""The argument vector length (${dfVals.length}) is different than the receiver vector length ($dfTypeLengthStr)."""
              )
          Val(dfType)(dfVals)
      end DFVectorCompareDFValVector
    end Compare
    object Ops:
      import IntP.{-, +}
      given evOpApplyDFVector[
          T <: DFTypeAny,
          D1 <: IntP,
          M <: ModifierAny,
          L <: DFVal[DFVector[T, Tuple1[D1]], M],
          R
      ](using
          ub: DFUInt.Val.UBArg[D1, R]
      ): ExactOp2Aux["apply", DFC, DFValAny, L, R, DFVal[T, M]] =
        new ExactOp2["apply", DFC, DFValAny, L, R]:
          type Out = DFVal[T, M]
          def apply(lhs: L, idx: R)(using DFC): Out = trydf {
            DFVal.Alias.ApplyIdx(
              lhs.dfType.cellType,
              lhs,
              ub(lhs.dfType.lengthIntParam, idx)(using dfc.anonymize)
            )
          }(using dfc, CTName("cell selection (apply)"))
      end evOpApplyDFVector
      given evOpApplyRangeDFVector[
          T <: DFTypeAny,
          D1 <: IntP,
          M <: ModifierAny,
          L <: DFVal[DFVector[T, Tuple1[D1]], M],
          LO <: IntP,
          HI <: IntP
      ](using
          checkLow: BitIndex.CheckNUB[LO, D1],
          checkHigh: BitIndex.CheckNUB[HI, D1],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, LO, HI, DFVal[
        DFVector[T, Tuple1[HI - LO + 1]],
        M
      ]] =
        new ExactOp3["apply", DFC, DFValAny, L, LO, HI]:
          type Out = DFVal[DFVector[T, Tuple1[HI - LO + 1]], M]
          def apply(lhs: L, idxLow: LO, idxHigh: HI)(using DFC): Out = trydf {
            val idxLowParam = IntParam(idxLow)
            val idxHighParam = IntParam(idxHigh)
            val idxLowIntOpt = idxLowParam.toScalaIntOpt
            val idxHighIntOpt = idxHighParam.toScalaIntOpt
            val lengthIntOpt = lhs.dfType.lengthIntOpt
            (idxLowIntOpt, lengthIntOpt) match
              case (Some(idxLowInt), Some(lengthInt)) => checkLow(idxLowInt, lengthInt)
              case _                                  =>
            (idxHighIntOpt, lengthIntOpt) match
              case (Some(idxHighInt), Some(lengthInt)) => checkHigh(idxHighInt, lengthInt)
              case _                                   =>
            (idxHighIntOpt, idxLowIntOpt) match
              case (Some(idxHighInt), Some(idxLowInt)) => checkHiLo(idxHighInt, idxLowInt)
              case _                                   =>
            DFVal.Alias.ApplyRange.applyVector(lhs, idxHighParam, idxLowParam)
          }(using dfc, CTName("cell range selection (apply)"))
      end evOpApplyRangeDFVector

      object DFVector:
        def apply[T <: DFTypeAny, D1 <: IntP, P](vectorType: DFVector[T, Tuple1[D1]])(
            elems: DFValTP[T, P]*
        )(using DFCG): DFValTP[DFVector[T, Tuple1[D1]], P] = trydf:
          if (elems.size == 1)
            DFVal.Func(
              vectorType,
              FuncOp.repeat,
              List(elems.head, vectorType.lengthIntParam.toDFConst)
            ).asValTP[DFVector[T, Tuple1[D1]], P]
          else
            vectorType.lengthIntOpt match
              case Some(ll) =>
                assert(
                  elems.size == ll,
                  "The number of elements in the vector does not match the vector length."
                )
              case None =>

            DFVal.Func(vectorType, FuncOp.++, elems.toList)
      end DFVector

      extension [T <: DFTypeAny, D1 <: IntP, M <: ModifierAny](
          lhs: DFVal[DFVector[T, Tuple1[D1]], M]
      )
        def elements(using DFC): Vector[DFValOf[T]] =
          import DFDecimal.StrInterpOps.d
          val elementType = lhs.dfType.cellType
          val elementNum = lhs.dfType.lengthIntOpt.getOrElse {
            throw new IllegalStateException(
              "Cannot get the elements of a vector with an unknown length."
            )
          }
          Vector.tabulate(elementNum)(i =>
            val idxVal = DFConstInt32(i)
            DFVal.Alias.ApplyIdx(elementType, lhs, idxVal)(using dfc.anonymize)
          )
        def length(using DFC): DFConstInt32 = lhs.dfType.lengthIntParam.toDFConst
      end extension
      extension (str: String)
        def toByteVector(using dfc: DFC): DFConstOf[DFVector[DFBits[8], Tuple1[Int]]] =
          val dfType = dfhdl.core.DFVector(DFBits(8), List(IntParam.fromValue(str.length)))
          val data = str.getBytes("ASCII").map(byte => (BitVector(byte), BitVector.low(8))).toVector
          DFVal.Const.forced(dfType, data, named = true)
    end Ops
  end Val
end DFVector
