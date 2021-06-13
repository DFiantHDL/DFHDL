package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFVector[T <: DFType, D <: NonEmptyTuple] <: DFType.Of[
  ir.DFVector
] =
  DFType.Of[ir.DFVector]

object DFVector extends DFVectorCompanion:
  def apply[T <: DFType, D <: NonEmptyTuple](
      cellType: T,
      cellDims: D
  ): DFVector[T, D] =
    ir.DFVector(cellType.asIR, cellDims.toList.asInstanceOf[List[Int]])
      .asInstanceOf[DFVector[T, D]]

trait DFVectorCompanion:
  object Ops:
    extension [T <: DFType.Supported](t: T)(using tc: TC[T])
      // transparent inline def X(inline cellDim: Int*): DFType =
      //   x(dfType, cellDim: _*)
      inline def X(
          inline cellDim: Int
      ): DFVector[tc.Type, Tuple1[cellDim.type]] =
        DFVector(tc(t), Tuple1(cellDim))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int
      ): DFVector[tc.Type, Tuple2[cellDim0.type, cellDim1.type]] =
        DFVector(tc(t), Tuple2(cellDim0, cellDim1))
      inline def X(
          inline cellDim0: Int,
          inline cellDim1: Int,
          inline cellDim2: Int
      ): DFVector[tc.Type, Tuple3[
        cellDim0.type,
        cellDim1.type,
        cellDim2.type
      ]] =
        DFVector(tc(t), Tuple3(cellDim0, cellDim1, cellDim2))
// transparent inline def x[T <: DFType](
//     cellType: T,
//     inline cellDim: Int*
// ): DFType =
//   ${ xMacro('cellType, 'cellDim) }
// def xMacro[T <: DFType](cellType: Expr[T], cellDim: Expr[Seq[Int]])(using
//     Quotes,
//     Type[T]
// ): Expr[DFType] =
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
