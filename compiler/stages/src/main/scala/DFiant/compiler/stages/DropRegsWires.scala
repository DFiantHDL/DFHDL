package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DomainType.RT.{NoClock, WithClock, ClockParams, NoReset, ResetParams, WithReset}
import DFiant.compiler.patching.*
import DFiant.internals.*
import scala.collection.mutable

private class DropRegsWires(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    // need to order members so that ports are at the beginning
    super.preTransform
  override def transform: DB =
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (owner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        owner.domainType match
          // only care about high-level domains.
          // those have wires and regs that we need to simplify.
          case domainType: DomainType.RT.HL =>
            val wires = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier == DFVal.Modifier.WIRE => dcl
            }
            val regs = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier == DFVal.Modifier.REG => dcl
            }
            val regVars = regs.map(_.copy(modifier = DFVal.Modifier.VAR))
            val regsPatch = regs
              .lazyZip(regVars)
              .map((r, rv) => r -> Patch.Replace(rv, Patch.Replace.Config.FullReplacement))
            // name and existence indicators for the clock and reset
            val hasClock = domainType.clkParams != NoClock
            val clkName = domainType.clkParams match
              case WithClock(name, _) => name
              case _                  => ""
            val hasReset = domainType.rstParams != NoReset
            val rstName = domainType.rstParams match
              case WithReset(name, _, _) => name
              case _                     => ""
            if (regs.nonEmpty)
              assert(
                hasClock,
                s"Clock is missing in high-level domain owner ${owner.getFullName}. Found registers but no clock is defined."
              )
            // if the assigned declaration is at an `always` block, then this is a blocking assignment.
            // otherwise, this is a non-blocking assignment.
//        toVal.dealias.get.getOwnerNamed match
//          case _: AlwaysBlock => csAssignmentOp
//          case _              => csNBAssignmentOp

            // adding clock and reset ports according to the domain configuration
            val clkRstPortsDsn = new MetaDesign:
              lazy val clk = DFBit <> IN setName clkName
              if (hasClock) clk // touch lazy clk to create
              lazy val rst = DFBit <> IN setName rstName
              if (hasReset) rst // touch lazy rst to create
            val addClkRstPatchOption =
              if (hasClock || hasReset)
                Some(owner -> Patch.Add(clkRstPortsDsn, Patch.Add.Config.InsideFirst))
              else None

            // changing the owner from high-level RT domain to low-level RT domain
            val updatedOwner = owner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.RT.LL())
              case domain: DomainBlock   => domain.copy(domainType = DomainType.RT.LL())
            val ownerDomainPatch =
              owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            var wiresPatch = List.empty[(DFMember, Patch)]
            var regs_dinPatch = List.empty[(DFMember, Patch)]
            val alwaysBlockDsn = new MetaDesign:
              val regs_dinVars = regs.map { r =>
                r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din")).asIR
              }
              regs_dinPatch = regs.lazyZip(regs_dinVars).flatMap { (r, r_din_v) =>
                members.collect {
                  case reg_din: DFVal.Alias.RegDIN if reg_din.relValRef.get == r =>
                    reg_din -> Patch.Replace(r_din_v, Patch.Replace.Config.ChangeRefAndRemove)
                }
              }
              always.all {
                wiresPatch = wires.map { w =>
                  w -> Patch.Replace(
                    w.asValAny.genNewVar(using dfc.setName(w.name)).asIR,
                    Patch.Replace.Config.ChangeRefAndRemove
                  )
                }
              }
              import DFiant.core.DFIf
              import DFiant.core.NoType
              import DFiant.core.{asTokenOf, DFTypeAny, DFOwnerAny}
              def regInitBlock() = regVars.foreach {
                case r if r.externalInit.nonEmpty =>
                  r.asVarAny := DFiant.core.DFVal.Const(
                    r.externalInit.get.head.asTokenOf[DFTypeAny]
                  )
                case _ =>
              }
              def regSaveBlock() = regs.lazyZip(regs_dinVars).foreach { (r, r_din_v) =>
                r.asVarAny := r_din_v.asValAny
              }
              def ifRstActive =
                import clkRstPortsDsn.rst
                val WithReset(_, _, active) = domainType.rstParams
                val cond = active match
                  case ResetParams.Active.High => rst == 1
                  case ResetParams.Active.Low  => rst == 0
                DFIf.singleBranch(Some(cond), DFIf.Header(NoType), regInitBlock)
              def ifClkEdge(ifRstOption: Option[DFOwnerAny]) =
                import clkRstPortsDsn.clk
                val WithClock(_, edge) = domainType.clkParams
                val cond = edge match
                  case ClockParams.Edge.Rising  => clk.rising
                  case ClockParams.Edge.Falling => clk.falling
                DFIf.singleBranch(
                  Some(cond),
                  ifRstOption.getOrElse(DFIf.Header(NoType)),
                  regSaveBlock
                )

              if (hasClock)
                import clkRstPortsDsn.clk
                if (hasReset)
                  val WithReset(_, mode, _) = domainType.rstParams
                  import clkRstPortsDsn.rst
                  mode match
                    case ResetParams.Mode.Sync =>
                      always(clk) {
                        ifClkEdge(None)
                      }
                    case ResetParams.Mode.Async =>
                      always(clk, rst) {
                        val (_, rstBranch) = ifRstActive
                        ifClkEdge(Some(rstBranch))
                      }
                else always(clk) { ifClkEdge(None) }
                end if
              end if

            val abOwnerIR = alwaysBlockDsn.getDB.members.collectFirst { case ab: AlwaysBlock =>
              ab
            }.get
            val alwaysBlockAllPatch =
              owner -> Patch.Add(alwaysBlockDsn, Patch.Add.Config.InsideLast)
            val alwaysBlockAllMembers = members.filter {
              case dcl: DFVal.Dcl     => false
              case dsn: DFOwner.Named => false
              case m                  => true
            }
            val alwaysBlockMembersPatch =
              abOwnerIR -> Patch.Move(
                alwaysBlockAllMembers,
                Patch.Move.Config.InsideLast
              )
            List(
              Some(ownerDomainPatch),
              addClkRstPatchOption,
              Some(alwaysBlockAllPatch),
              Some(alwaysBlockMembersPatch),
              wiresPatch,
              regsPatch,
              regs_dinPatch
            ).flatten
          // other domains
          case _ => None
      // other owners
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end DropRegsWires

extension [T: HasDB](t: T) def dropRegsWires: DB = new DropRegsWires(t.db).transform
