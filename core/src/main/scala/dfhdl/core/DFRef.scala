package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.analysis.DclPort

import scala.annotation.targetName
extension [M <: ir.DFMember](member: M)
  private[dfhdl] def injectGlobalCtx()(using DFC): Unit =
    import dfc.getSet
    member match
      case dfVal: ir.DFVal.CanBeGlobal if dfVal.isGlobal =>
        given CanEqual[Any, Null] = CanEqual.derived
        if (dfVal.globalCtx != null)
          dfc.mutableDB.injectGlobals(
            dfVal.globalCtx.asInstanceOf[DesignContext]
          )
      case _ =>
  end injectGlobalCtx
  // due to user meta-programming, it's possible that the user attempts to reference "unreachable"
  // values within a certain design. this method attempts to create reachable members instead of
  // limiting the user capabilities during elaboration.
  private[core] def getReachableMember(using dfc: DFC): M =
    import dfc.getSet
    member match
      case dcl: ir.DFVal.Dcl               => member
      case pbns: ir.DFVal.PortByNameSelect => member
      // only unreachable members are values, and we disable this mechanism during compiler stage
      // meta-programming since there we reference values outside of the design context entirely.
      case dfVal: ir.DFVal if !dfc.inMetaProgramming && !dfVal.isGlobal =>
        dfc.ownerOption match
          case Some(currentOwner) => dfVal.cloneUnreachable.asInstanceOf[M]
          case _                  => member
      case _ => member
    end match
  end getReachableMember

  // True if the member is a port declaration of a design other than the one currently being
  // elaborated, e.g. a sub-design instance's port accessed as `inst.port` from the parent.
  // Such a port has no member of its own in the current design context: this design
  // represents it by a `PortByNameSelect` (see `foreignPortSelectOpt`), so it can be
  // referenced from here but never revised in place, since `MutableDB.setMember` looks the
  // original member up in the current design context and would not find it.
  private[core] def isForeignPort(using dfc: DFC): Boolean =
    import dfc.getSet
    member match
      // in meta-programming we can end up with a modified copy of the design that should
      // not be treated as a different design (for example, the stage `ToED`).
      // for this reason we only compare the owner references which are guaranteed to be
      // different for different design, but not for a copy made during meta-programming step.
      case port @ DclPort() =>
        port.getOwnerDesign.ownerRef != dfc.owner.asIR.getThisOrOwnerDesign.ownerRef
      case _ => false

  // The current design's `PortByNameSelect` representative of a foreign port (see
  // `isForeignPort`), planted as a member here. That representative is what a reference to
  // the port materializes (see `refTW`) and what a tag applied to the port from here lands
  // on (see `DFVal.revisableHere`).
  // Returns `None` for any member the current design context holds directly.
  private[core] def foreignPortSelectOpt(using dfc: DFC): Option[ir.DFVal.PortByNameSelect] =
    import dfc.getSet
    member match
      case port: ir.DFVal.Dcl if port.isForeignPort =>
        // name path accounts for domains within the design that can contain the port
        val namePath = port.getRelativeName(port.getOwnerDesign)
        Some(
          DFVal.PortByNameSelect(
            port.dfType,
            port.modifier.dir,
            port.getOwnerDesign.getCachedDesignInst,
            namePath
          )
        )
      case _ => None
    end match
  end foreignPortSelectOpt

  def ref(using DFC): ir.DFRef.OneWay[M] =
    val newRef = dfc.refGen.genOneWay[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW[O <: ir.DFMember](using dfc: DFC): ir.DFRef.TwoWay[M, O] =
    refTW[O](knownReachable = false)
  def refTW[O <: ir.DFMember](knownReachable: Boolean)(using dfc: DFC): ir.DFRef.TwoWay[M, O] =
    import dfc.getSet
    injectGlobalCtx()
    val reachableMember = if (knownReachable) member else member.getReachableMember
    reachableMember.foreignPortSelectOpt match
      // referencing a port from another design causes by-name referencing
      case Some(portSelect) =>
        portSelect.refTW[O].asInstanceOf[ir.DFRef.TwoWay[M, O]]
      // any other kind of reference
      case None =>
        val newRef = dfc.refGen.genTwoWay[M, O]
        dfc.mutableDB.newRefFor(newRef, reachableMember)
    end match
  end refTW
end extension

extension [T <: ir.DFOwner](owner: DFOwner[T])
  def ref(using DFC): ir.DFRef.OneWay[T] =
    owner.asIR.ref
