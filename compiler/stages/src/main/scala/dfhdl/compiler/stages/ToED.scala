package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.History.Op as HistoryOp
import dfhdl.core.{DFC, DFIf, DFOwnerAny}
import scala.annotation.tailrec

case object ToED extends Stage:
  def dependencies: List[Stage] =
    List(ToRT, NameRegAliases, ExplicitNamedVars, AddClkRst, SimpleOrderMembers)
  def nullifies: Set[Stage] = Set()
  case class RegNet(
      net: DFNet,
      regVar: DFVal.Dcl,
      regAlias: DFVal.Alias.History,
      relVal: DFVal,
      initOption: Option[DFTokenAny]
  )
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val domainAnalysis = new DomainAnalysis(designDB)
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (owner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        owner.domainType match
          // only care about register-transfer domains.
          // those have wires and regs that we need to simplify.
          case domainType @ DomainType.RT(cfg @ RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
            val clkRstOpt = domainAnalysis.designDomains((owner.getThisOrOwnerDesign, cfg))

            // changing the owner from RT domain to ED domain
            val updatedOwner = owner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.ED)
              case domain: DomainBlock   => domain.copy(domainType = DomainType.ED)
            val ownerDomainPatch =
              owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            val regNets = members.collect:
              case net @ DFNet.Assignment(
                    regVar @ DclVar(),
                    regAlias @ DFVal.Alias.History(
                      _,
                      DFRef(relVal),
                      _,
                      HistoryOp.Reg,
                      initOption,
                      _,
                      _,
                      _
                    )
                  ) =>
                RegNet(net, regVar, regAlias, relVal, initOption)
            val removedNets = regNets.view.map(rn => rn.net).toSet
            val regAliasRemovalPatches = regNets.flatMap(rn =>
              List(
                rn.net -> Patch.Remove,
                rn.regAlias -> Patch.Remove
              )
            )
            @tailrec def getDeps(
                leftMembers: List[DFMember],
                handledMembers: Set[DFMember]
            ): Set[DFMember] =
              leftMembers match
                case head :: last =>
                  head match
                    case dcl: DFVal.Dcl => getDeps(last, handledMembers)
                    case _ if !handledMembers.contains(head) =>
                      getDeps(last ++ head.getRefs.map(_.get), handledMembers + head)
                    case _ => getDeps(last, handledMembers)
                case Nil => handledMembers
            val processBlockAllMembersSet = members.view.flatMap {
              case net: DFNet if net.isConnection || removedNets.contains(net) => None
              case net: DFNet =>
                getDeps(List(net), Set())
              case ch: DFConditional.Header if ch.dfType == DFUnit =>
                getDeps(List(ch), Set())
              case _ => None
            }.toSet
            // println(processBlockAllMembersSet.mkString("\n"))
            val processBlockAllMembers = members.flatMap {
              case dcl: DFVal.Dcl => None
              case cb: DFConditional.Block if cb.getHeaderCB.dfType == DFUnit =>
                cb :: designDB.blockMemberTable(cb)
              case dsn: DFOwnerNamed                          => None
              case history: DFVal.Alias.History               => None
              case m if processBlockAllMembersSet.contains(m) => Some(m)
              case _                                          => None
            }

            val processBlocksDsn =
              new MetaDesign(updatedOwner, Patch.Add.Config.InsideLast, domainType = DFC.Domain.ED):
                lazy val clk = clkRstOpt.clkOpt.get.asValOf[Bit]
                lazy val rst = clkRstOpt.rstOpt.get.asValOf[Bit]

                // create a combinational process if needed
                if (processBlockAllMembers.nonEmpty)
                  process(all) {}
                def regInitBlock() = regNets.foreach:
                  case rn if rn.initOption.nonEmpty && !rn.initOption.get.isBubble =>
                    rn.regVar.asVarAny :== dfhdl.core.DFVal.Const(rn.initOption.get.asTokenAny)
                  case _ =>
                def regSaveBlock() = regNets.foreach: rn =>
                  rn.regVar.asVarAny :== rn.relVal.asValAny
                def ifRstActive =
                  val RstCfg.Explicit(_, active: RstCfg.Active) = rstCfg: @unchecked
                  val cond = active match
                    case RstCfg.Active.High => rst == 1
                    case RstCfg.Active.Low  => rst == 0
                  DFIf.singleBranch(Some(cond), DFIf.Header(dfhdl.core.DFUnit), regInitBlock)
                def ifRstActiveElseRegSaveBlock(): Unit =
                  val (_, rstBranch) = ifRstActive
                  DFIf.singleBranch(None, rstBranch, regSaveBlock)
                def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                  val ClkCfg.Explicit(edge: ClkCfg.Edge) = clkCfg: @unchecked
                  val cond = edge match
                    case ClkCfg.Edge.Rising  => clk.rising
                    case ClkCfg.Edge.Falling => clk.falling
                  DFIf.singleBranch(
                    Some(cond),
                    ifRstOption.getOrElse(DFIf.Header(dfhdl.core.DFUnit)),
                    block
                  )

                if (clkCfg != None && regNets.nonEmpty)
                  if (
                    rstCfg != None && regNets.exists(rn =>
                      rn.initOption.nonEmpty && !rn.initOption.get.isBubble
                    )
                  )
                    val RstCfg.Explicit(mode: RstCfg.Mode, _) = rstCfg: @unchecked
                    mode match
                      case RstCfg.Mode.Sync =>
                        process(clk) {
                          ifClkEdge(None, ifRstActiveElseRegSaveBlock)
                        }
                      case RstCfg.Mode.Async =>
                        process(clk, rst) {
                          val (_, rstBranch) = ifRstActive
                          ifClkEdge(Some(rstBranch))
                        }
                  else process(clk) { ifClkEdge(None) }
                  end if
                end if

            val processBlockAllMembersPatchOption = if (processBlockAllMembers.nonEmpty)
              val pbAllOwner = processBlocksDsn.getDB.members.collectFirst {
                case pb: ProcessBlock =>
                  pb
              }.get
              Some(
                pbAllOwner -> Patch.Move(
                  processBlockAllMembers,
                  owner,
                  Patch.Move.Config.InsideLast
                )
              )
            else None
            List(
              Some(ownerDomainPatch),
              Some(owner -> Patch.Add(processBlocksDsn, Patch.Add.Config.InsideLast)),
              processBlockAllMembersPatchOption,
              regAliasRemovalPatches
            ).flatten
          // other domains
          case _ => None
        end match
      // other owners
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end ToED

extension [T: HasDB](t: T) def toED(using CompilerOptions): DB = StageRunner.run(ToED)(t.db)
