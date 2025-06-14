package dfhdl.lib.algebra
import dfhdl.*
import dfhdl.internals.ClassEv
import scala.annotation.targetName
//because we define our own `apply` here and still use the dfhdl.apply, we need to export it here
//to prevent ambiguities. See: https://contributors.scala-lang.org/t/relaxed-extension-methods-sip-54-are-not-relaxed-enough/6585
export dfhdl.apply
//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Matrix Data Structure
//////////////////////////////////////////////////////////////////////////////////////////////////////
//TODO: fix if https://github.com/lampepfl/dotty/issues/17036 is resolved

abstract class Column[ET <: DFType, RN <: Int & Singleton](
    val elemType: ET,
    val rowNum: RN
) extends Opaque[ET X RN](elemType X rowNum)
extension [ET <: DFType, RN <: Int & Singleton, CT <: Column[ET, RN]](
    col: CT <> VAL
)(using ClassEv[CT])
  def colType: CT = col.opaqueType
  def rowNum: RN = colType.rowNum
  def elemType: ET = colType.elemType
  @inline def apply(rowIdx: Int): ET <> DFRET = col.actual(rowIdx)
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
  @targetName("matApply")
  @inline def apply(colIdx: Int): CT <> DFRET = matrix.actual(colIdx)
  @inline def apply(rowIdx: Int, colIdx: Int): ET <> DFRET = matrix.actual(colIdx).actual(rowIdx)
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
