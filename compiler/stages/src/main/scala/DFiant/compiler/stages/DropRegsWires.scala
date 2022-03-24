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
            val alwaysBlockAllDsn = new MetaDesign:
              regs_dinPatch = regs.flatMap { r =>
                val reg_dinVar = r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din")).asIR
                members.collect {
                  case reg_din: DFVal.Alias.RegDIN if reg_din.relValRef.get == r =>
                    reg_din -> Patch.Replace(reg_dinVar, Patch.Replace.Config.ChangeRefAndRemove)
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
              always(clkRstPortsDsn.clk, clkRstPortsDsn.rst) {
                val (_, rstBranch) =
                  DFIf.singleBranch(Some(clkRstPortsDsn.rst), DFIf.Header(NoType), () => {})
                DFIf.singleBranch(Some(clkRstPortsDsn.clk.rising), rstBranch, () => {})
              }

            val abOwnerIR = alwaysBlockAllDsn.getDB.members.collectFirst { case ab: AlwaysBlock =>
              ab
            }.get
            val alwaysBlockAllPatch =
              owner -> Patch.Add(alwaysBlockAllDsn, Patch.Add.Config.InsideLast)
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
