package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import collection.immutable.ListMap

type DFStruct[+F <: DFFields] = OpaqueDFStruct.DFStruct[F]
val DFStruct = OpaqueDFStruct.DFStruct

private object OpaqueDFStruct:
  opaque type DFStruct[+F <: DFFields] <: DFType.Of[ir.DFStruct] =
    DFType.Of[ir.DFStruct]
  object DFStruct:
    def apply[F <: DFFields](fields: F): DFStruct[F] =
      val fieldMap = ListMap(
        fields.getFields.map(f => (f.name, f.dfType.asIR)): _*
      )
      ir.DFStruct(fields.name, fieldMap).asFE[DFStruct[F]]
