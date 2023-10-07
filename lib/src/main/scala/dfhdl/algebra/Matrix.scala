package dfhdl.lib.algebra
import dfhdl.{apply => _, *}
export dfhdl.apply
//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Matrix Data Structure
//////////////////////////////////////////////////////////////////////////////////////////////////////
//TODO: fix if https://github.com/lampepfl/dotty/issues/17036 is resolved

abstract class Column[ET <: core.DFTypeAny, RN <: Int with Singleton](
    val elemType: ET,
    val rowNum: RN
) extends Opaque[ET X RN](elemType X rowNum)
extension [ET <: core.DFTypeAny, RN <: Int with Singleton, T <: Column[ET, RN]](
    col: T <> VAL
)(using ce: dfhdl.internals.ClassEv[T])
  @inline def mapElements(f: ET <> VAL => ET <> VAL): T <> VAL = col.actual.elements.as(ce.value)

abstract class Matrix[
    CN <: Int with Singleton,
    ET <: core.DFTypeAny,
    RN <: Int with Singleton,
    CFE <: Column[ET, RN]
](
    val colFE: CFE,
    val colNum: CN
) extends Opaque[CFE X CN](colFE X colNum)
extension [
    CN <: Int with Singleton,
    ET <: core.DFTypeAny,
    RN <: Int with Singleton,
    CFE <: Column[ET, RN],
    M <: Matrix[CN, ET, RN, CFE]
](matrix: M <> VAL)(using cfe: dfhdl.internals.ClassEv[CFE], m: dfhdl.internals.ClassEv[M])
  @inline def apply(colIdx: Int): CFE <> VAL = matrix.actual(colIdx)
  @inline def apply(rowIdx: Int, colIdx: Int): ET <> VAL = matrix.actual(colIdx).actual(rowIdx)
  @inline def mapElementsViaIndexes(f: (Int, Int) => ET <> VAL): M <> VAL =
    Vector
      .tabulate(m.value.colNum, m.value.colFE.rowNum)(f)
      .map(_.as(cfe.value)).as(m.value)
  @inline def mapColumnsViaIndex(f: Int => Vector[ET <> VAL]): M <> VAL =
    Vector
      .tabulate(m.value.colNum)(x => f(x).as(cfe.value))
      .as(m.value)
end extension
