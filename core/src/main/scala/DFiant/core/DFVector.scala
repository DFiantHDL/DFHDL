package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import DFVal.Func.Op as FuncOp
import DFiant.compiler.ir.DFVal.Modifier
import DFiant.core.CompanionsDFBits.BitIndex

import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance

type DFVector[+T <: DFTypeAny, +D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T @uncheckedVariance, D @uncheckedVariance]]

object DFVector:
  def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: D
  ): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.toList.asInstanceOf[List[Int]])
      .asFE[DFVector[T, D]]

  extension [T <: DFTypeAny, D <: NonEmptyTuple](dfType: DFVector[T, D])
    def cellType: T = dfType.asIR.cellType.asFE[T]
    def cellDims: List[Int] = dfType.asIR.cellDims

  protected[core] object IndexWidth
      extends Check2[
        Int,
        Int,
        [IW <: Int, W <: Int] =>> IW == W,
        [IW <: Int, W <: Int] =>> "The index width " + IW +
          " is different than the expected width of the vector address " + W
      ]

  object Ops:
    extension [T <: DFType.Supported](t: T)(using tc: DFType.TC[T])
      // transparent inline def X(inline cellDim: Int*): DFType =
      //   x(dfType, cellDim: _*)
      inline def X(
          inline cellDim: Int
      ): DFVector[tc.Type, Tuple1[cellDim.type]] =
        DFVector(tc(t), Tuple1(cellDim))
//      inline def X(
//          inline cellDim0: Int,
//          inline cellDim1: Int
//      ): DFVector[tc.Type, Tuple2[cellDim0.type, cellDim1.type]] =
//        DFVector(tc(t), Tuple2(cellDim0, cellDim1))
//      inline def X(
//          inline cellDim0: Int,
//          inline cellDim1: Int,
//          inline cellDim2: Int
//      ): DFVector[tc.Type, Tuple3[
//        cellDim0.type,
//        cellDim1.type,
//        cellDim2.type
//      ]] =
//        DFVector(tc(t), Tuple3(cellDim0, cellDim1, cellDim2))
    end extension
  end Ops

  type Token[+T <: DFTypeAny, +D <: NonEmptyTuple] = DFToken[DFVector[T, D]]
  object Token:
    def apply[T <: DFTypeAny, D <: NonEmptyTuple](
        dfType: DFVector[T, D],
        data: Vector[Any]
    ): Token[T, D] =
      val dim = dfType.asIR.cellDims.head
      assert(
        data.length == dim,
        s"The length of the Scala vector (${data.length}) does not match the dataflow vector dimension ($dim)"
      )
      ir.DFVector.Token(dfType.asIR, data).asTokenOf[DFVector[T, D]]

    object TC:
      import DFToken.TC
      given DFVectorTokenFromVector[T <: DFTypeAny, D1 <: Int, R](using
          tc: TC[T, R]
      ): TC[DFVector[T, Tuple1[D1]], Vector[R]] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: Vector[R]): Out =
          Token(dfType, arg.map(tc(dfType.cellType, _).asIR.data))
      given DFVectorTokenFromSEV[T <: DFTypeAny, D1 <: Int, R](using
          tc: TC[T, R]
      ): TC[DFVector[T, Tuple1[D1]], SameElementsVector[R]] with
        def conv(
            dfType: DFVector[T, Tuple1[D1]],
            arg: SameElementsVector[R]
        ): Out =
          Token(
            dfType,
            Vector.fill(dfType.cellDims.head)(
              tc(dfType.cellType, arg.value).asIR.data
            )
          )
    end TC

    object Compare:
      import DFToken.Compare
      given DFVectorCompare[
          T <: DFTypeAny,
          D1 <: Int,
          R,
          Op <: FuncOp,
          C <: Boolean
      ](using
          tc: Compare[T, R, Op, C],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], Vector[R], Op, C] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: Vector[R]): Out =
          Token(dfType, arg.map(tc.conv(dfType.cellType, _).asIR.data))
    end Compare
    object Ops:
      extension [T <: DFTypeAny, D1 <: Int](lhs: Token[T, Tuple1[D1]])
        @targetName("applyDFVector")
        def apply[I <: Int](
            idx: Inlined[I]
        )(using
            check: BitIndex.Check[I, D1]
        ): DFToken[T] =
          check(idx, lhs.dfType.cellDims.head)
          ir.DFToken
            .forced(lhs.dfType.asIR.cellType, lhs.data(idx))
            .asTokenOf[T]
  end Token

  object Val:
    object TC:
      import DFVal.TC
      given DFVectorValFromDFValVector[
          T <: DFTypeAny,
          D1 <: Int,
          R
      ](using
          tc: TC[T, R],
          dfc: DFC
      ): TC[DFVector[T, Tuple1[D1]], Vector[R]] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: Vector[R]): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)
      given DFVectorValFromSEV[
          T <: DFTypeAny,
          D1 <: Int,
          R
      ](using
          tc: TC[T, R],
          dfc: DFC
      ): TC[DFVector[T, Tuple1[D1]], SameElementsVector[R]] with
        def conv(
            dfType: DFVector[T, Tuple1[D1]],
            arg: SameElementsVector[R]
        ): Out =
          val dfVals =
            List.fill(dfType.cellDims.head)(tc.conv(dfType.cellType, arg.value))
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      end DFVectorValFromSEV
    end TC
    object Compare:
      import DFVal.Compare
      given DFVectorCompareDFValVector[
          T <: DFTypeAny,
          D1 <: Int,
          R,
          Op <: FuncOp,
          C <: Boolean
      ](using
          dfc: DFC,
          tc: Compare[T, R, Op, C],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[DFVector[T, Tuple1[D1]], Vector[R], Op, C] with
        def conv(dfType: DFVector[T, Tuple1[D1]], arg: Vector[R]): Out =
          val dfVals = arg.view.map(tc.conv(dfType.cellType, _)).toList
          DFVal.Func(dfType, FuncOp.++, dfVals)(using dfc.anonymize)
      end DFVectorCompareDFValVector
    end Compare
    object Ops:
      extension [T <: DFTypeAny, D1 <: Int, M <: Modifier](
          lhs: DFVal[DFVector[T, Tuple1[D1]], M]
      )
        @targetName("applyDFVector")
        def apply[I <: Int](
            idx: Inlined[I]
        )(using
            dfc: DFC,
            check: BitIndex.Check[I, D1]
        ): DFVal[T, M] =
          check(idx, lhs.dfType.cellDims.head)
          DFVal.Alias.ApplyIdx(lhs.dfType.cellType, lhs, idx)
        @targetName("applyDFVector")
        def apply[IW <: Int](
            idx: DFUInt[IW] <> VAL
        )(using info: IntInfo[D1])(using
            check: IndexWidth.Check[IW, info.OutW],
            dfc: DFC
        ): DFVal[T, M] =
          check(idx.width, info.width(lhs.dfType.cellDims.head))
          DFVal.Alias.ApplyIdx(lhs.dfType.cellType, lhs, idx)
      end extension
    end Ops
  end Val
end DFVector
