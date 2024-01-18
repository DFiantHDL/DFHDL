package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.analysis.DclPort

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  private def injectGlobalCtx()(using DFC): Unit =
    member match
      case dfVal: ir.DFVal.CanBeGlobal if dfVal.isGlobal =>
        dfc.mutableDB.injectGlobals(
          dfVal.globalCtx.asInstanceOf[DesignContext]
        )
      case _ =>
  def ref(using DFC, ClassTag[M]): ir.DFRef.OneWay[M] =
    val newRef = new ir.DFRef.OneWay[M]:
      lazy val refType = classTag[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW[O <: ir.DFMember](
      originMember: => O
  )(using dfc: DFC, m: ClassTag[M], o: ClassTag[O]): ir.DFRef.TwoWay[M, O] =
    import dfc.getSet
    injectGlobalCtx()
    lazy val newOriginRef = originMember.ref
    member match
      // referencing a port from another design causes by-name referencing.
      // in meta-programming we can end up with a modified copy of the design that should
      // not be treated as a different design (for example, the stage `ToED`).
      // for this reason we only compare the owner references which are guaranteed to be
      // different for different design, but not for a copy made during meta-programming step.
      case port @ DclPort()
          if port.getOwnerDesign.ownerRef != dfc.owner.asIR.getThisOrOwnerDesign.ownerRef =>
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
