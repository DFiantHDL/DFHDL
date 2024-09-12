package dfhdl.app
import dfhdl.*
import core.{DFValAny, asValAny}
import scala.collection.immutable.ListMap

case class DesignArg(name: String, typeName: String, value: Any, desc: String)(using DFC):
  def getScalaValue: Any =
    val data = value match
      case dfConst: DFValAny =>
        import dfc.getSet
        dfConst.asIR.getConstData.asInstanceOf[Option[Option[Any]]].get.get
      case _ => value
    data match
      case bigInt: BigInt => bigInt.toInt
      case _              => data
  def updateScalaValue(updatedScalaValue: Any): DesignArg =
    val updatedData = updatedScalaValue match
      case int: Int if value.isInstanceOf[DFValAny] => BigInt(int)
      case _                                        => updatedScalaValue
    val updatedValue = value match
      case dfConst: DFValAny =>
        core.DFVal.Const.forced(dfConst.dfType, Some(updatedData))
      case _ => updatedData
    copy(value = updatedValue)
end DesignArg

type DesignArgs = ListMap[String, DesignArg]
object DesignArgs:
  def empty: DesignArgs = ListMap.empty
  def apply(
      argNames: List[String],
      argTypes: List[String],
      argValues: List[Any],
      argDescs: List[String]
  ): DesignArgs =
    ListMap.from(
      argNames.lazyZip(argTypes).lazyZip(argValues).lazyZip(argDescs).map(
        (name, typeName, value, desc) => name -> DesignArg(name, typeName, value, desc)
      )
    )
  def apply(iter: Iterable[(String, DesignArg)]): DesignArgs = ListMap.from(iter)
end DesignArgs
