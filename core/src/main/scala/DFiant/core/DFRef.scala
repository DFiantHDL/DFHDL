package DFiant.core
import DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

extension [M <: ir.DFMember](member: M)
  def ref(using ClassTag[M], DFC): ir.DFRef.OneWay[M] = ???
  def refTW: ir.DFRef.TwoWay[M] = ???
