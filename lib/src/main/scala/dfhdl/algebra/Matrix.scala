package dfhdl.lib.algebra
import dfhdl.{apply => _, *}
import scala.annotation.targetName
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
)
  def colType: CT = col.opaqueType
  def rowNum: RN = colType.rowNum
  def elemType: ET = colType.elemType
  @inline def mapElements(f: ET <> VAL => ET <> VAL): CT <> DFRET =
    col.actual.elements.map(f).as(colType)

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
](matrix: MT <> VAL)
  def matType: MT = matrix.opaqueType
  @targetName("matColType")
  def colType: CT = matType.colType
  @targetName("matRowNum")
  def rowNum: RN = colType.rowNum
  def colNum: CN = matType.colNum
  @inline def apply(colIdx: Int): CT <> DFRET = matrix.actual(colIdx)
  @inline def apply(rowIdx: Int, colIdx: Int): ET <> DFRET = matrix.actual(colIdx).actual(rowIdx)
  @inline def mapElementsViaIndexes(f: (Int, Int) => ET <> VAL): MT <> DFRET =
    Vector
      .tabulate(colNum, rowNum)(f)
      .map(_.as(colType)).as(matType)
  @inline def mapColumnsViaIndex(f: Int => Vector[ET <> VAL]): MT <> DFRET =
    Vector
      .tabulate(colNum)(x => f(x).as(colType))
      .as(matType)
end extension
