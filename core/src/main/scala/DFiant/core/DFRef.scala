package DFiant.core
import DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  def ref(using ClassTag[M], DFC): ir.DFRef.OneWay[M] =
    val newRef = new ir.DFRef.OneWay[M]:
      lazy val refType = classTag[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW[O <: ir.DFMember](
      originMember: => O
  )(using ClassTag[M], DFC): ir.DFRef.TwoWay[M] = ???
