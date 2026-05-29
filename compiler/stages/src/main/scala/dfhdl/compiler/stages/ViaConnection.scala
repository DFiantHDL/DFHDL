package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable

case object ViaConnection extends HierarchyStage:
  def dependencies: List[Stage] = List(DropDesignDefs, ExplicitNamedVars, SimpleOrderMembers)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patchList: List[(DFMember, Patch)] = subDB.members.flatMap {
      case ib: DFDesignInst =>
        val nets = mutable.ListBuffer.empty[DFNet]
        val pbnsSkip = mutable.Set.empty[DFVal.PortByNameSelect]
        val pbnsGrps =
          subDB.designInstPBNS.getOrElse(ib, Nil).groupBy(_.portNamePath).values.toList
        pbnsGrps.foreach { pbnss =>
          // all connection nets driving (input) or driven by (output) this port group.
          // a port may be wired by several nets, each covering a disjoint bit range.
          val groupNets =
            pbnss.flatMap(p => if (p.isOut) p.getConnectionsFrom else p.getConnectionsTo).toSet
          // a port already wired through via-connections (possibly several partial ones)
          // is left untouched
          if (groupNets.nonEmpty && groupNets.forall(_.isViaConnection))
            pbnss.foreach(pbnsSkip += _)
          else
            pbnss match
              // a single PBNS that is not already a via-connection
              case List(pbns) =>
                if (pbns.isOut)
                  pbns.getConnectionsFrom.headOption match
                    // connected to OPEN, so we skip it from variable creation, but add the net to
                    // the list of nets to be moved as via connections
                    case Some(n @ DFNet.Connection(toVal = openVal: DFVal.Special))
                        if openVal.isOpen =>
                      pbnsSkip += pbns
                      nets += n
                    // already connected to a variable, but without via connection, so we add the
                    // net to the list of nets to be moved as via connections
                    case Some(n @ DFNet.Connection(toVal = DclVar())) =>
                      pbnsSkip += pbns
                      nets += n
                    // not connected through a net
                    case _ => // do nothing
                  end match
                else // input port
                  pbns.getConnectionsTo.toList match
                    // we have a single net that is assigned not more than once (otherwise, for RTL purposes we require another value so an internal multi-assignment rtl variable/reg can be assigned into a signal/wire)
                    case (n @ DFNet.Connection(fromVal = v @ DclVar())) :: Nil
                        if v.getAssignmentsTo.isEmpty =>
                      nets += n
                      pbnsSkip += pbns
                    // not connected through a net
                    case _ => // do nothing
                  end match
              // multiple PBNS (all belong to the same port) that are not via-connected are
              // bridged through a synthesized variable
              case _ => // do nothing
            end match
        }

        // Meta design to construct the variables to be connected to the ports
        val addVarsDsn = new MetaDesign(ib, Patch.Add.Config.Before):
          val pbnssToVars: List[(List[DFVal.PortByNameSelect], DFVal)] =
            pbnsGrps.flatMap {
              case pbnss @ (pbns :: _) if !pbnsSkip.contains(pbns) =>
                // p.setReachableTypes()
                val name = s"${ib.getName}_${pbns.portNamePath.replace('.', '_')}"
                Some(pbnss -> pbns.asValAny.genNewVar(using dfc.setName(name)).asIR)
              case _ => Nil
            }
        // Meta design for connections between ports and the added variables.
        // Anchored after the DFDesignInst (rather than the DFDesignBlock) so
        // any synthesized PBNS that targets the inst sits AFTER it in the
        // flat member list — preserves the order check invariant.
        val connectDsn = new MetaDesign(ib, Patch.Add.Config.After):
          dfc.mutableDB.injectMetaGetSet(addVarsDsn.getDB.getSet)
          dfc.enterLate()
          val refPatches: List[(DFMember, Patch)] = addVarsDsn.pbnssToVars.flatMap {
            case (pbnss @ (pbns :: _), v) =>
              val newPBNS = pbns.cloneAnonValueAndDepsHere.asDclAny
              if (pbns.isOut) v.asDclAny.<>(newPBNS)(using dfc.setMeta(pbns.meta))
              else newPBNS.<>(v.asValAny)(using dfc.setMeta(pbns.meta))
              // the old external by-name port selector needs to be removed
              // and its references set to the new design "wiring" variables
              pbnss.map(
                _ -> Patch.Replace(v, Patch.Replace.Config.ChangeRefAndRemove)
              )
            case _ => Nil
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
          }.toList
        addVarsDsn.patch :: connectDsn.patch :: connectDsn.refPatches ++ connectDsn.movedNets
      case _ => Nil
    }
    subDB.patch(patchList)
  end transformSubDB
end ViaConnection

extension [T: HasDB](t: T)
  def viaConnection(using CompilerOptions): DB =
    StageRunner.run(ViaConnection)(t.db)
