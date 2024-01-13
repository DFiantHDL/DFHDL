package dfhdl.core
import dfhdl.compiler.ir

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  def ref(using DFC, ClassTag[M]): ir.DFRef.OneWay[M] =
    val newRef = new ir.DFRef.OneWay[M]:
      lazy val refType = classTag[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW[O <: ir.DFMember](
      originMember: => O
  )(using dfc: DFC, m: ClassTag[M], o: ClassTag[O]): ir.DFRef.TwoWay[M, O] =
    import dfc.getSet
    lazy val newOriginRef = originMember.ref
    member match
      // referencing a port from another design causes by-name referencing
      case port: ir.DFVal.Dcl
          if port.isPort && port.getOwnerDesign != dfc.owner.asIR.getThisOrOwnerDesign =>
        // name path accounts for domains within the design that can contain the port
        val namePath = port.getRelativeName(port.getOwnerDesign)
        lazy val portSelect: ir.DFVal.PortByNameSelect = ir.DFVal.PortByNameSelect(
          port.dfType,
          port.getOwnerDesign.refTW(portSelect),
          namePath,
          dfc.owner.ref,
          dfc.getMeta.anonymize,
          ir.DFTags.empty
        )
        portSelect.addMember.refTW(originMember).asInstanceOf[ir.DFRef.TwoWay[M, O]]
      // any other kind of reference
      case _ =>
        val newRef = new ir.DFRef.TwoWay[M, O]:
          lazy val refType = classTag[M]
          lazy val originRef = newOriginRef
        dfc.mutableDB.newRefFor(newRef, member)
    end match
  end refTW
end extension

extension [T <: ir.DFOwner](owner: DFOwner[T])
  def ref(using ClassTag[T], DFC): ir.DFRef.OneWay[T] =
    owner.asIR.ref
