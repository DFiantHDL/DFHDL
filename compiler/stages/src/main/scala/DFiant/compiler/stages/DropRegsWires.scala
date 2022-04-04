package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.{*, given}
import DFiant.compiler.patching.*
import DFiant.internals.*
import scala.collection.mutable

private class DropRegsWires(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    // need to order members so that ports are at the beginning
    // need ViaPortConnection
    // need derived clock and reset configuration to be explicit
    super.preTransform
  override def transform: DB =
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (owner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        owner.domainType match
          // only care about register-transfer domains.
          // those have wires and regs that we need to simplify.
          case domainType: DomainType.RT =>
            val wires = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier == DFVal.Modifier.WIRE => dcl
            }
            val regs = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier == DFVal.Modifier.REG => dcl
            }
            if (regs.nonEmpty)
              assert(
                domainType.clkCfg != None,
                s"Clock is missing in high-level domain owner ${owner.getFullName}. Found registers but no clock is defined."
              )
            val regVars = regs.map(_.copy(modifier = DFVal.Modifier.VAR))
            val regsPatch = regs
              .lazyZip(regVars)
              .map((r, rv) => r -> Patch.Replace(rv, Patch.Replace.Config.FullReplacement))
            // name and existence indicators for the clock and reset
            val hasClock = domainType.clkCfg != None
            val clkName = domainType.clkCfg match
              case ClkCfg.Explicit(name: String, _) => name
              case _                                => ""
            val hasReset = domainType.rstCfg != None
            val rstName = domainType.rstCfg match
              case RstCfg.Explicit(name: String, _, _) => name
              case _                                   => ""

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

            // changing the owner from RT domain to ED domain
            val updatedOwner = owner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.ED())
              case domain: DomainBlock   => domain.copy(domainType = DomainType.ED())
            val ownerDomainPatch =
              owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            var wiresPatch = List.empty[(DFMember, Patch)]
            var regs_dinPatch = List.empty[(DFMember, Patch)]
            var regs_din_vPatch = List.empty[(DFMember, Patch)]
            val alwaysBlockDsn = new MetaDesign:
              val regs_dinVars = regs.map { r =>
                r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din")).asIR
              }
              always.all {
                wiresPatch = wires.map { w =>
                  w -> Patch.Replace(
                    w.asValAny.genNewVar(using dfc.setName(w.name)).asIR,
                    Patch.Replace.Config.ChangeRefAndRemove
                  )
                }
                regs_dinPatch = regs.lazyZip(regs_dinVars).flatMap { (r, r_din_v) =>
                  members.collect {
                    case reg_din: DFVal.Alias.RegDIN if reg_din.relValRef.get == r =>
                      reg_din -> Patch.Replace(r_din_v, Patch.Replace.Config.ChangeRefAndRemove)
                  }
                }
//                regs_din_vPatch = regs.map { r =>
//                  r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din")).asIR
//                }
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
                val RstCfg.Explicit(_, _, active) = domainType.rstCfg
                val cond = active match
                  case RstCfg.Active.High => rst == 1
                  case RstCfg.Active.Low  => rst == 0
                DFIf.singleBranch(Some(cond), DFIf.Header(NoType), regInitBlock)
              def ifRstActiveElseRegSaveBlock(): Unit =
                val (_, rstBranch) = ifRstActive
                DFIf.singleBranch(None, rstBranch, regSaveBlock)
              def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                import clkRstPortsDsn.clk
                val ClkCfg.Explicit(_, edge) = domainType.clkCfg
                val cond = edge match
                  case ClkCfg.Edge.Rising  => clk.rising
                  case ClkCfg.Edge.Falling => clk.falling
                DFIf.singleBranch(
                  Some(cond),
                  ifRstOption.getOrElse(DFIf.Header(NoType)),
                  block
                )

              if (hasClock)
                import clkRstPortsDsn.clk
                if (hasReset)
                  val RstCfg.Explicit(_, mode, _) = domainType.rstCfg
                  import clkRstPortsDsn.rst
                  mode match
                    case RstCfg.Mode.Sync =>
                      always(clk) {
                        ifClkEdge(None, ifRstActiveElseRegSaveBlock)
                      }
                    case RstCfg.Mode.Async =>
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
              case dcl: DFVal.Dcl                     => false
              case dsn: DFOwner.Named                 => false
              case net: DFNet if net.lateConstruction => false
              case m                                  => true
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
