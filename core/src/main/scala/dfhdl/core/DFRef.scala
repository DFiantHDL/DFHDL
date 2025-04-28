package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.analysis.DclPort

import scala.annotation.targetName
import scala.reflect.{ClassTag, classTag}
extension [M <: ir.DFMember](member: M)
  private def injectGlobalCtx()(using DFC): Unit =
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
  private[core] def getReachableMember(using dfc: DFC): ir.DFMember =
    import dfc.getSet
    member match
      // only unreachable members are values, and we disable this mechanism during compiler stage
      // meta-programming since there we reference values outside of the design context entirely.
      case dfVal: ir.DFVal if !dfc.inMetaProgramming =>
        dfc.ownerOption match
          case Some(currentOwner) =>
            // one case of unreachable value is when the value is global and anonymous.
            // in this case we clone the entire chain of anonymous dependencies.
            if (dfVal.isGlobal)
              if (dfVal.isAnonymous) dfVal.cloneAnonValueAndDepsHere
              else dfVal
            // another case of unreachable value is when attempting to reference a value of an owner
            // design.
            else if (currentOwner.asIR.getThisOrOwnerDesign.isOneLevelBelow(dfVal.getOwnerDesign))
              // if the value is anonymous, we clone it with its entire anonymous dependency.
              if (dfVal.isAnonymous) dfVal.cloneAnonValueAndDepsHere
              // if the value is named, we need to create a design parameter to propagate the
              // named value into the design. since this can happen more than once, we memoize
              // the named values we access and fetch the created design parameter accordingly.
              else
                dfc.mutableDB.DesignContext.getReachableNamedValue(
                  dfVal,
                  DFVal.DesignParam(dfVal.asValAny)(using dfc.setMeta(dfVal.meta).emptyTags).asIR
                )
            else member
          case _ => member
      case _ => member
    end match
  end getReachableMember

  def ref(using DFC, ClassTag[M]): ir.DFRef.OneWay[M] =
    val newRef = new ir.DFRef.OneWay[M]:
      val refType = classTag[M]
    dfc.mutableDB.newRefFor(newRef, member)
  def refTW[O <: ir.DFMember](using
      dfc: DFC,
      m: ClassTag[M],
      o: ClassTag[O]
  ): ir.DFRef.TwoWay[M, O] = refTW[O](knownReachable = false)
  def refTW[O <: ir.DFMember](knownReachable: Boolean)(using
      dfc: DFC,
      m: ClassTag[M],
      o: ClassTag[O]
  ): ir.DFRef.TwoWay[M, O] =
    import dfc.getSet
    injectGlobalCtx()
    val reachableMember = if (knownReachable) member else member.getReachableMember
    reachableMember match
      // referencing a port from another design causes by-name referencing.
      // in meta-programming we can end up with a modified copy of the design that should
      // not be treated as a different design (for example, the stage `ToED`).
      // for this reason we only compare the owner references which are guaranteed to be
      // different for different design, but not for a copy made during meta-programming step.
      case port @ DclPort()
          if port.getOwnerDesign.ownerRef != dfc.owner.asIR.getThisOrOwnerDesign.ownerRef =>
        // name path accounts for domains within the design that can contain the port
        val namePath = port.getRelativeName(port.getOwnerDesign)
        val portSelect: ir.DFVal.PortByNameSelect = ir.DFVal.PortByNameSelect(
          port.dfType.dropUnreachableRefs,
          port.getOwnerDesign.refTW[ir.DFVal.PortByNameSelect],
          namePath,
          dfc.owner.ref,
          dfc.getMeta.anonymize,
          dfc.tags
        )
        portSelect.addMember.refTW[O].asInstanceOf[ir.DFRef.TwoWay[M, O]]
      // any other kind of reference
      case member =>
        val newRef = new ir.DFRef.TwoWay[M, O]:
          val refType = classTag[M]
          val originRefType = classTag[O]
        dfc.mutableDB.newRefFor(newRef, member)
    end match
  end refTW
end extension

extension [T <: ir.DFOwner](owner: DFOwner[T])
  def ref(using ClassTag[T], DFC): ir.DFRef.OneWay[T] =
    owner.asIR.ref
