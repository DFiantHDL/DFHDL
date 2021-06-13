package DFiant
package core
import internals.*

import scala.collection.mutable

/////////////////////////////////////////////////////////////////////////////
// DFFields are used for either struct or enumerations (tagged unions)
/////////////////////////////////////////////////////////////////////////////
abstract class DFFields(using meta: MetaContext) extends Product, Serializable:
  final private val all =
    mutable.ListBuffer.empty[DFField[_ <: DFType]]
  final lazy val getFields = all.toList
  final val name: String = meta.clsNameOpt.get
  protected sealed trait FIELD
  protected object FIELD extends FIELD
  extension [T <: DFType.Supported](t: T)(using tc: TC[T])
    def <>(FIELD: FIELD)(using MetaContext): DFField[tc.Type] =
      val dfType = tc(t)
      val field = DFField(dfType)
      all += field
      field
final case class DFField[Type <: DFType](dfType: Type)(using
    meta: MetaContext
):
  val name: String = meta.name
/////////////////////////////////////////////////////////////////////////////
