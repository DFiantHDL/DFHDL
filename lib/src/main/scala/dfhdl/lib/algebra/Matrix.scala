package dfhdl.lib.algebra
import dfhdl.*
import dfhdl.internals.*
import scala.annotation.targetName
import dfhdl.core.DFValAny
//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Matrix Data Structure
//////////////////////////////////////////////////////////////////////////////////////////////////////
//TODO: fix if https://github.com/lampepfl/dotty/issues/17036 is resolved

abstract class Column[ET <: DFType, RN <: Int & Singleton](
    val elemType: ET,
    val rowNum: RN
) extends Opaque[ET X RN](elemType X rowNum)
object Column:
  given [ET <: DFType, RN <: Int & Singleton, CT <: Column[ET, RN], L <: CT <> VAL, RI <: Int](using
      ClassEv[CT]
  ): ExactOp2Aux["apply", DFC, DFValAny, L, RI, ET <> VAL] =
    new ExactOp2["apply", DFC, DFValAny, L, RI]:
      type Out = ET <> VAL
      def apply(lhs: L, rowIdx: RI)(using DFC): Out = lhs.actual(rowIdx)
end Column

extension [ET <: DFType, RN <: Int & Singleton, CT <: Column[ET, RN]](
    col: CT <> VAL
)(using ClassEv[CT])
  def colType: CT = col.opaqueType
  def rowNum: RN = colType.rowNum
  def elemType: ET = colType.elemType
  @inline def mapElems(f: ET <> VAL => ET <> VAL): CT <> DFRET =
    col.actual.elements.map(f).as(colType)
  @inline def zipMapElems(rhs: CT <> VAL)(f: (ET <> VAL, ET <> VAL) => ET <> VAL): CT <> DFRET =
    col.actual.elements.lazyZip(rhs.actual.elements).map(f).as(colType)
extension [ET <: DFType, RN <: Int & Singleton, CT <: Column[ET, RN]](
    colType: CT
)
  @inline def tabulateElems(f: Int => ET <> VAL): CT <> DFRET =
    Vector.tabulate(colType.rowNum)(x => f(x)).as(colType)

abstract class Matrix[
    CN <: Int & Singleton,
    ET <: DFType,
    RN <: Int & Singleton,
    CT <: Column[ET, RN]
](
    val colType: CT,
    val colNum: CN
) extends Opaque[CT X CN](colType X colNum)
object Matrix:
  given [
      CN <: Int & Singleton,
      ET <: DFType,
      RN <: Int & Singleton,
      CT <: Column[ET, RN],
      MT <: Matrix[CN, ET, RN, CT],
      L <: MT <> VAL,
      CI <: Int
  ](using
      ClassEv[MT]
  ): ExactOp2Aux["apply", DFC, DFValAny, L, CI, CT <> VAL] =
    new ExactOp2["apply", DFC, DFValAny, L, CI]:
      type Out = CT <> VAL
      def apply(lhs: L, colIdx: CI)(using DFC): Out = lhs.actual(colIdx)
  given [
      CN <: Int & Singleton,
      ET <: DFType,
      RN <: Int & Singleton,
      CT <: Column[ET, RN],
      MT <: Matrix[CN, ET, RN, CT],
      L <: MT <> VAL,
      RI <: Int,
      CI <: Int
  ](using
      ClassEv[MT]
  ): ExactOp3Aux["apply", DFC, DFValAny, L, RI, CI, ET <> VAL] =
    new ExactOp3["apply", DFC, DFValAny, L, RI, CI]:
      type Out = ET <> VAL
      def apply(lhs: L, rowIdx: RI, colIdx: CI)(using DFC): Out = lhs.actual(colIdx).actual(rowIdx)
end Matrix

extension [
    CN <: Int & Singleton,
    ET <: DFType,
    RN <: Int & Singleton,
    CT <: Column[ET, RN],
    MT <: Matrix[CN, ET, RN, CT]
](matrix: MT <> VAL)(using ClassEv[MT])
  def matType: MT = matrix.opaqueType
  @targetName("matColType")
  def colType: CT = matType.colType
  @targetName("matRowNum")
  def rowNum: RN = colType.rowNum
  def colNum: CN = matType.colNum
end extension
extension [
    CN <: Int & Singleton,
    ET <: DFType,
    RN <: Int & Singleton,
    CT <: Column[ET, RN],
    MT <: Matrix[CN, ET, RN, CT]
](matType: MT)
  def rowNum: RN = matType.colType.rowNum
  // tabulating with (row, col) index order, to match the apply method indexing
  @inline def tabulateElems(f: (Int, Int) => ET <> VAL): MT <> DFRET =
    // the given tabulation function needs to be flipped for proper construction order
    // of columns then rows
    @inline def fixedF(colIdx: Int, rowIdx: Int): ET <> VAL = f(rowIdx, colIdx)
    Vector
      .tabulate(matType.colNum, rowNum)(fixedF)
      .map(_.as(matType.colType)).as(matType)
  @inline def tabulateCols(f: Int => Vector[ET <> VAL]): MT <> DFRET =
    Vector
      .tabulate(matType.colNum)(x => f(x).as(matType.colType))
      .as(matType)
end extension
