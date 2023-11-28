package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

val DFUnit = new DFType[ir.DFUnit, NoArgs](ir.DFUnit)
type DFUnit = DFType[ir.DFUnit, NoArgs]
object DFUnitToken:
  def apply(): DFToken[DFUnit] = ir.DFToken(ir.DFUnit)(()).asTokenOf[DFUnit]

object DFUnitVal:
  def apply()(using DFC): DFValOf[DFUnit] = DFVal.Const(DFUnitToken())
