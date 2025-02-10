package dfhdl.app
import dfhdl.*
import dfhdl.compiler.ir
import core.{DFValAny, asValAny}
import scala.collection.immutable.ListMap

case class DesignArg(name: String, value: Any, desc: String)(using DFC):
  val typeName =
    value match
      case _: String  => "String"
      case _: Int     => "Int"
      case _: Double  => "Double"
      case _: Boolean => "Boolean"
      case _: BigInt  => "Int"
      case dfConst: DFValAny =>
        dfConst.asIR.dfType match
          case ir.DFBit | ir.DFBool => "Boolean"
          case ir.DFInt32           => "Int"
          case _                    => ""
      case _ => ""
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
      argValues: List[Any],
      argDescs: List[String]
  ): DesignArgs =
    ListMap.from(
      argNames.lazyZip(argValues).lazyZip(argDescs).map((name, value, desc) =>
        name -> DesignArg(name, value, desc)
      )
    )
  def apply(iter: Iterable[(String, DesignArg)]): DesignArgs = ListMap.from(iter)
end DesignArgs
