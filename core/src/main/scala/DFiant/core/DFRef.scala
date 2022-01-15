package DFiant.core
import DFiant.compiler.ir

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  def ref(using ClassTag[M], DFC): ir.DFRef.OneWay[M] =
    val newRef = new ir.DFRef.OneWay[M]:
      lazy val refType = classTag[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW(
      originMember: => ir.DFMember
  )(using ClassTag[M], DFC): ir.DFRef.TwoWay[M] =
    lazy val newOriginRef = originMember.ref
    val newRef = new ir.DFRef.TwoWay[M]:
      lazy val refType = classTag[M]
      lazy val originRef = newOriginRef
    dfc.mutableDB.newRefFor(newRef, member)

extension [T <: ir.DFOwner](owner: DFOwner[T])
  def ref(using ClassTag[T], DFC): ir.DFRef.OneWay[T] =
    owner.asIR.ref
