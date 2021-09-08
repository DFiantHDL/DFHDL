package DFiant
package core
import internals.*

import scala.collection.mutable

/////////////////////////////////////////////////////////////////////////////
// DFFields are used for either struct or enumerations (tagged unions)
/////////////////////////////////////////////////////////////////////////////
abstract class DFFields extends Product, Serializable, HasTypeName:
  final private val all =
    mutable.ListBuffer.empty[DFField[_ <: DFType]]
  final lazy val getFields = all.toList
  final lazy val name: String = typeName
  protected sealed trait FIELD
  protected object FIELD extends FIELD
  private[core] def createField[T <: DFType](
      dfType: T,
      name: String
  ): DFField[T] =
    val field = DFField(dfType, name)
    all += field
    field

  extension [T <: DFType.Supported](t: T)(using tc: DFType.TC[T])
    def <>(FIELD: FIELD)(using ctName: CTName): DFField[tc.Type] =
      val dfType = tc(t)
      createField(dfType, ctName.value)
end DFFields
final case class DFField[Type <: DFType](dfType: Type, name: String)
/////////////////////////////////////////////////////////////////////////////
