package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

type DFVector[T <: DFTypeAny, D <: NonEmptyTuple] =
  DFType[ir.DFVector, Args2[T, D]]

object DFVector:
  def apply[T <: DFTypeAny, D <: NonEmptyTuple](
      cellType: T,
      cellDims: D
  ): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.toList.asInstanceOf[List[Int]])
      .asFE[DFVector[T, D]]

  object Ops:
    extension [T <: DFType.Supported](t: T)
      // transparent inline def X(inline cellDim: Int*): DFType =
      //   x(dfType, cellDim: _*)
      inline def X(
          inline cellDim: Int
      ): DFVector[DFType.Of[T], Tuple1[cellDim.type]] =
        DFVector(DFType.of(t), Tuple1(cellDim))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int
      ): DFVector[DFType.Of[T], Tuple2[cellDim0.type, cellDim1.type]] =
        DFVector(DFType.of(t), Tuple2(cellDim0, cellDim1))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int,
          inline cellDim2: Int
      ): DFVector[DFType.Of[T], Tuple3[
        cellDim0.type,
        cellDim1.type,
        cellDim2.type
      ]] =
        DFVector(DFType.of(t), Tuple3(cellDim0, cellDim1, cellDim2))
    end extension
  end Ops
end DFVector

// transparent inline def x[T <: DFTypeAny](
//     cellType: T,
//     inline cellDim: Int*
// ): DFType =
//   ${ xMacro('cellType, 'cellDim) }
// def xMacro[T <: DFTypeAny](cellType: Expr[T], cellDim: Expr[Seq[Int]])(using
//     Quotes,
//     Type[T]
// ): Expr[DFTypeAny] =
//   import quotes.reflect.*
//   val (tpe, tpl) = cellDim match
//     case Varargs(argExprs) =>
//       argExprs match
//         case arg :: Nil =>
//           println(arg)
//           val tp = ConstantType(IntConstant(5)).asType
//           (
//             TypeRepr.of[Tuple1[5]].asType.asInstanceOf[Type[Int]],
//             '{ Tuple1($arg) }
//           )
//   '{ new DFVector[T, tpe.Underlying]($cellType, $tpl) }
