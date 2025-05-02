package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*

case object ViaConnection extends Stage:
  def dependencies: List[Stage] = List(DropDesignDefs, ExplicitNamedVars)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      case (ib, members) if !ib.isTop =>
        // getting only ports that are not already connected to variables
        val (ports, nets): (List[DFVal.Dcl], List[DFNet]) =
          members.foldRight((List.empty[DFVal.Dcl], List.empty[DFNet])) {
            case (p @ DclOut(), (ports, nets)) =>
              val conns = p.getConnectionsFrom
              conns.headOption match
                case Some(n) if n.isViaConnection =>
                  (ports, nets) // already has via connections
                // connected to OPEN, so we skip it from variable creation
                case Some(n @ DFNet.Connection(toVal = _: DFVal.OPEN)) =>
                  (ports, n :: nets)
                case Some(n @ DFNet.Connection(toVal = DclVar())) if conns.size == 1 =>
                  (ports, n :: nets)
                // output ports that are not used are skipped and not via-connected
                case None if p.getReadDeps.forall(_.isInsideOwner(ib)) =>
                  (ports, nets)
                case _ => (p :: ports, nets)
            case (p @ DclIn(), (ports, nets)) =>
              p.getConnectionTo match
                // we have a single net that is assigned not more than once
                // (otherwise, for RTL purposes we require another value so an internal multi-assignment rtl variable/reg
                // can be assigned into a signal/wire)
                case Some(n) if n.isViaConnection =>
                  (ports, nets) // already has via connections
                case Some(n @ DFNet.Connection(fromVal = v @ DclVar()))
                    if v.getAssignmentsTo.isEmpty =>
                  (ports, n :: nets)
                case _ => (p :: ports, nets)
            case (_, x) => x
          }
        // Meta design to construct the variables to be connected to the ports
        val addVarsDsn = new MetaDesign(ib, Patch.Add.Config.Before):
          val portsToVars: List[(DFVal.Dcl, DFVal)] = ports.map { p =>
            p -> p.asValAny.genNewVar(using dfc.setName(s"${ib.getName}_${p.getName}")).asIR
          }
        // Meta design for connections between ports and the added variables
        val connectDsn = new MetaDesign(ib, Patch.Add.Config.After):
          dfc.mutableDB.injectMetaGetSet(addVarsDsn.getDB.getSet)
          dfc.enterLate()
          val refPatches: List[(DFMember, Patch)] = addVarsDsn.portsToVars.flatMap { case (p, v) =>
            val pbns = p.getPortsByNameSelectors
            if (pbns.nonEmpty)
              val portMeta = pbns.head.meta
              p match
                case _ @DclOut() => v.asDclAny.<>(p.asValAny)(using dfc.setMeta(portMeta))
                case _ @DclIn()  => p.asDclAny.<>(v.asValAny)(using dfc.setMeta(portMeta))
                case _           => ???
            // the old external by-name port selector needs to be removed
            // and its references set to the new design "wiring" variables
            pbns.map(
              _ -> Patch.Replace(v, Patch.Replace.Config.ChangeRefAndRemove)
            )
          }
          val movedNets: List[(DFMember, Patch)] = nets.flatMap { n =>
            // plant the net as via net
            val viaNet = plantMember(n.copy(op = DFNet.Op.ViaConnection))
            val changeToViaPatch =
              n -> Patch.Replace(viaNet, Patch.Replace.Config.ChangeRefAndRemove)
            val relMembers = n.collectRelMembers
            // relevant members must move alongside the net
            if (relMembers.nonEmpty)
              List(
                changeToViaPatch,
                viaNet -> Patch.Move(relMembers, n.getOwnerDesign, Patch.Move.Config.Before)
              )
            else List(changeToViaPatch)
          }
        addVarsDsn.patch :: connectDsn.patch :: connectDsn.refPatches ++ connectDsn.movedNets
      case _ => Nil
    }
    designDB.patch(patchList)
  end transform
end ViaConnection

extension [T: HasDB](t: T)
  def viaConnection(using CompilerOptions): DB =
    StageRunner.run(ViaConnection)(t.db)
