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
      dfc: DFC,
      cellType: T,
      d: ValueOf[D],
      check: VectorLength.CheckNUB[D]
  ): DFVector[T, Tuple1[D]] = trydf:
    val cellDim = IntParam.fromValue(d)
    check(cellDim)
    DFVector(cellType, List(cellDim))

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = dfType.asIR.cellType.asFE[T]
  extension [T <: DFTypeAny, D1 <: IntP](dfType: DFVector[T, Tuple1[D1]])
    def lengthInt(using dfc: DFC): Int =
      import dfc.getSet
      dfType.asIR.cellDimParamRefs.head.getInt
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

  sealed class ComposedModifier[D <: IntP, M](val cellDim: D, val modifier: M)
  object Ops:
    extension [T <: DFType.Supported, D <: IntP](t: T)(using tc: DFType.TC[T])
      infix def X(
          cellDim: IntParam[D]
      )(using dfc: DFC, check: VectorLength.CheckNUB[D]): DFVector[tc.Type, Tuple1[D]] = trydf:
        check(cellDim)
        DFVector[tc.Type, Tuple1[D]](tc(t), List(cellDim))
    extension [T <: DFType.Supported, D <: IntP, M <: ModifierAny](t: T)(using tc: DFType.TC[T])
      infix def X(
          composedModifier: ComposedModifier[D, M]
      )(using dfc: DFC, check: VectorLength.CheckNUB[D]): DFVal[DFVector[tc.Type, Tuple1[D]], M] =
        trydf:
          val cellDim = IntParam.fromValue(composedModifier.cellDim)
          check(cellDim)
          DFVal.Dcl(DFVector[tc.Type, Tuple1[D]](tc(t), List(cellDim)), composedModifier.modifier)
    end extension
  end Ops

  object Val:
    def apply[T <: DFTypeAny, D1 <: IntP, P](vectorType: DFVector[T, Tuple1[D1]])(
        from: List[DFValTP[T, P]]
    )(using DFC): DFValTP[DFVector[T, Tuple1[D1]], P] =
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
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          import dfc.getSet
          given Printer = DefaultPrinter
          check(dfType.lengthInt, arg.dfType.lengthInt)
          if (dfType.asIR.isSimilarTo(arg.dfType.asIR))
            arg.asValTP[DFVector[T, Tuple1[D1]], RP]
          else
            throw new IllegalArgumentException(
              s"""|Vector types must be the same when applying one vector onto another.
                  |Expected type: ${dfType.codeString}
                  |Found type:    ${arg.dfType.codeString}""".stripMargin
            )
      end DFVectorValFromDFVectorVal
      given DFVectorValFromDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          TCE <: TC[T, E]
      ](using
          tc: TCE
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = tc.OutP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)(using dfc.anonymize)).toList
          val check = summon[`LL == RL`.Check[Int, Int]]
          check(dfType.lengthInt, dfVals.length)
          Val(dfType)(dfVals)
      end DFVectorValFromDFValVector
      given DFVectorValFromSEV[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: SameElementsVector[E],
          TCE <: TC[T, E]
      ](using
          tc: TCE
      ): TC[DFVector[T, Tuple1[D1]], R] with
        type OutP = tc.OutP
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
          R <: Iterable[E],
          TCE <: TCConv[T, E]
      ](using
          tc: TCE
      ): TCConv[DFVector[T, Tuple1[Int]], R] with
        type OutP = tc.OutP
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
          check(dfType.lengthInt, arg.dfType.lengthInt)
          if (dfType.asIR.isSimilarTo(arg.dfType.asIR))
            arg.asValTP[DFVector[T, Tuple1[D1]], RP]
          else
            throw new IllegalArgumentException(
              s"""|Vector types must be the same when comparing one vector to another.
                  |Expected type: ${dfType.codeString}
                  |Found type:    ${arg.dfType.codeString}""".stripMargin
            )
      end DFVectorCompareFromDFVectorCompare

      given DFVectorCompareDFValVector[
          T <: DFTypeAny,
          D1 <: IntP,
          E,
          R <: Iterable[E],
          Op <: FuncOp,
          C <: Boolean,
          TC <: Compare[T, E, Op, C]
      ](using
          tc: TC,
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], R, Op, C] with
        type OutP = tc.OutP
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: R)(using DFC): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)(using dfc.anonymize)).toList
          val check = summon[`LL == RL`.Check[Int, Int]]
          check(dfType.lengthInt, dfVals.length)
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
            checkLow(IntParam(idxLow), lhs.dfType.lengthIntParam)
            checkHigh(IntParam(idxHigh), lhs.dfType.lengthIntParam)
            checkHiLo(IntParam(idxHigh), IntParam(idxLow))
            DFVal.Alias.ApplyRange.applyVector(lhs, IntParam(idxHigh), IntParam(idxLow))
          }(using dfc, CTName("cell range selection (apply)"))
      end evOpApplyRangeDFVector

      object DFVector:
        def apply[T <: DFTypeAny, D1 <: IntP, P](vectorType: DFVector[T, Tuple1[D1]])(
            elems: DFValTP[T, P]*
        )(using DFC): DFValTP[DFVector[T, Tuple1[D1]], P] = trydf:
          if (elems.size == 1)
            DFVal.Func(
              vectorType,
              FuncOp.repeat,
              List(elems.head, vectorType.lengthIntParam.toDFConst)
            ).asValTP[DFVector[T, Tuple1[D1]], P]
          else
            assert(
              elems.size == dfhdl.core.DFVector.lengthInt(vectorType),
              "The number of elements in the vector does not match the vector length."
            )
            DFVal.Func(vectorType, FuncOp.++, elems.toList)
      end DFVector

      extension [T <: DFTypeAny, D1 <: IntP, M <: ModifierAny](
          lhs: DFVal[DFVector[T, Tuple1[D1]], M]
      )
        def elements(using DFC): Vector[DFValOf[T]] =
          import DFDecimal.StrInterpOps.d
          val elementType = lhs.dfType.cellType
          Vector.tabulate(lhs.lengthInt)(i =>
            val idxVal = DFConstInt32(i)
            DFVal.Alias.ApplyIdx(elementType, lhs, idxVal)(using dfc.anonymize)
          )
        def length(using DFC): DFConstInt32 = lhs.dfType.lengthIntParam.toDFConst
        def lengthInt(using DFC): Int = lhs.dfType.lengthIntParam.toScalaInt
      end extension
      extension (str: String)
        def toByteVector(using dfc: DFC): DFConstOf[DFVector[DFBits[8], Tuple1[Int]]] =
          val dfType = dfhdl.core.DFVector(DFBits(8), List(IntParam.fromValue(str.length)))
          val data = str.getBytes("ASCII").map(byte => (BitVector(byte), BitVector.low(8))).toVector
          DFVal.Const.forced(dfType, data, named = true)
    end Ops
  end Val
end DFVector
