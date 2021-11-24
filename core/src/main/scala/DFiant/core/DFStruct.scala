package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

import collection.immutable.ListMap
import scala.annotation.unchecked.uncheckedVariance

type DFStruct[+F <: DFFields] = DFType[ir.DFStruct, Args1[F @uncheckedVariance]]
object DFStruct:
  def apply[F <: DFFields](fields: F): DFStruct[F] =
    val fieldMap = ListMap(
      fields.getFields.map(f => (f.name, f.dfType.asIR))*
    )
    ir.DFStruct(fields.name, fieldMap).asFE[DFStruct[F]]
