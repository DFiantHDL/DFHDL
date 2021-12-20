package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import collection.immutable.ListMap
import scala.annotation.unchecked.uncheckedVariance

type DFStruct[+F <: Product] = DFType[ir.DFStruct, Args1[F @uncheckedVariance]]
object DFStruct:
  def apply[F <: Product](
      name: String,
      fieldMap: ListMap[String, DFTypeAny]
  ): DFStruct[F] =
    ir.DFStruct(name, fieldMap.map((n, t) => (n, t.asIR))).asFE[DFStruct[F]]
  def apply[F <: Product](
      name: String,
      fieldNames: List[String],
      fieldTypes: List[DFTypeAny]
  ): DFStruct[F] =
    apply[F](name, ListMap(fieldNames.lazyZip(fieldTypes).toSeq*))
  inline given [F <: Product]: DFStruct[F] = ${ dfTypeMacro[F] }
  def dfTypeMacro[F <: Product](using Quotes, Type[F]): Expr[DFStruct[F]] =
    import quotes.reflect.*
    val fTpe = TypeRepr.of[F]
    val clsSym = fTpe.classSymbol.get
    val fields =
      clsSym.fieldMembers.view
        .map(m => (m.name.toString, fTpe.memberType(m).asTypeOf[Any]))
    val structName = if (fTpe.isTupleN) "" else clsSym.name.toString

    val fieldErrors = fields.filter {
      case (_, '[DFValOf[t]]) => false
      case _                  => true
    }.toList
    if (fieldErrors.isEmpty)
      val fieldNames: List[Expr[String]] = fields.map((n, _) => Expr(n)).toList
      val fieldTypes: List[Expr[DFTypeAny]] = fields.collect {
        case (_, '[DFValOf[t]]) =>
          '{ compiletime.summonInline[t] }
      }.toList
//      println(fieldNames.map(_.show))
//      println(fieldTypes.map(_.show))
      val fieldNamesExpr = Varargs(fieldNames)
      val fieldTypesExpr = Varargs(fieldTypes)
      val nameExpr = Expr(structName)
      '{
        DFStruct
          .apply[F]($nameExpr, List($fieldNamesExpr*), List($fieldTypesExpr*))
      }
    else
      val fieldTypesStr = fieldErrors
        .map { case (n, '[t]) =>
          s"${n}: ${TypeRepr.of[t].showType}"
        }
        .mkString("\n")
      val msg =
        s"""The struct `$structName` has invalid dataflow value field types. 
           |A valid field type is in the form of [DFType] <> VAL.
           |The following fields do not match this pattern:
           |$fieldTypesStr""".stripMargin
      '{ compiletime.error(${ Expr(msg) }) }
    end if
  end dfTypeMacro
end DFStruct

//  def apply[F <: Product](fields: F): DFStruct[F] =
//    val fieldMap = ListMap(
//      fields.getFields.map(f => (f.name, f.dfType.asIR))*
//    )
//    ir.DFStruct(fields.name, fieldMap).asFE[DFStruct[F]]
