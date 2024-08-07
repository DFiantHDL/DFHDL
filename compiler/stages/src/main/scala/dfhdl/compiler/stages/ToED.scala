package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.History.Op as HistoryOp
import DFVal.Modifier
import dfhdl.core.{DFIf, DFOwnerAny, DFOpaque}
import dfhdl.core.DomainType.ED
import scala.annotation.tailrec
import scala.collection.mutable
case object ToED extends Stage:
  def dependencies: List[Stage] =
    List(ToRT, NameRegAliases, ExplicitNamedVars, AddClkRst, SimpleOrderMembers)
  def nullifies: Set[Stage] = Set()
  case class RegNet(
      net: DFNet,
      regVar: DFVal.Dcl,
      regAlias: DFVal.Alias.History,
      relVal: DFVal,
      initOption: Option[DFVal]
  )
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val domainAnalysis = new DomainAnalysis(designDB)
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (domainOwner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        object Config:
          def unapply(domainType: DomainType): Option[RTDomainCfg.Explicit] =
            domainType match
              case DomainType.RT(cfg: RTDomainCfg.Explicit) => Some(cfg)
              case DomainType.RT(RTDomainCfg.Related(ref))  => unapply(ref.get.domainType)
              case _                                        => None
        domainOwner.domainType match
          // only care about register-transfer domains.
          // those have wires and regs that we need to simplify.
          case domainType @ Config(cfg) =>
            import cfg.{clkCfg, rstCfg}
            val clkRstOpt = domainAnalysis.designDomains(domainOwner)

            val dclREGList = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier.isReg => dcl
            }

            // changing the owner from RT domain to ED domain
            val updatedOwner = domainOwner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.ED)
              case domain: DomainBlock   => domain.copy(domainType = DomainType.ED)
            val ownerDomainPatch =
              domainOwner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            val regNets = designDB.domainOwnerMemberTable(domainOwner.getThisOrOwnerDomain).collect:
              case net @ DFNet.Assignment(
                    regVar @ DclVar(),
                    regAlias @ DFVal.Alias.History(
                      _,
                      DFRef(relVal),
                      _,
                      HistoryOp.State,
                      _,
                      _,
                      _,
                      _
                    )
                  ) =>
                RegNet(net, regVar, regAlias, relVal, regAlias.initOption)
            val removedNets = regNets.view.map(rn => rn.net).toSet
            val regAliasRemovalPatches = regNets.flatMap(rn =>
              List(
                rn.net -> Patch.Remove(),
                rn.regAlias -> Patch.Remove()
              )
            )

            val assignCnt = mutable.Map.empty[DFVal.Dcl, Int]
            def anotherAssignCnt(toVal: DFVal): Unit =
              toVal.departialDcl.foreach {
                case (dcl: DFVal.Dcl, _) if !dcl.modifier.isReg =>
                  assignCnt.update(dcl, assignCnt.getOrElse(dcl, 0) + 1)
                case _ =>
              }
            def collectFilter(member: DFMember): Boolean = member match
              case _: DFVal.Dcl                               => false
              case DesignParam(_)                             => false
              case _: DFOwnerNamed                            => false
              case _: DFVal.Alias.History                     => false
              case _: DFVal.PortByNameSelect                  => false
              case dfVal: DFVal if dfVal.isReferencedByAnyDcl => false
              case _                                          => true

            def getProcessAllMembers(list: List[DFMember]): List[DFMember] =
              val processBlockAllMembersSet: Set[DFMember] = list.view.flatMap {
                case DesignParam(_)                 => None
                case net: DFNet if net.isConnection => None
                case net @ DFNet.Assignment(toVal, _) if !removedNets.contains(net) =>
                  anotherAssignCnt(toVal)
                  net :: net.collectRelMembers
                case ch: DFConditional.Header if ch.dfType == DFUnit =>
                  ch.collectRelMembers(false)
                case cb: DFConditional.Block =>
                  cb.guardRef.get match
                    case dfVal: DFVal => cb :: dfVal.collectRelMembers(false)
                    case _            => List(cb)
                case _ => None
              }.toSet

              list.view.filter(collectFilter).flatMap {
                case cb: DFConditional.Block if cb.getHeaderCB.dfType == DFUnit =>
                  cb :: getProcessAllMembers(designDB.blockMemberTable(cb))
                case m if processBlockAllMembersSet.contains(m) => Some(m)
                case _                                          => None
              }.toList
            end getProcessAllMembers
            val combinationalMembers = getProcessAllMembers(members)
            val singleAssignments = combinationalMembers.flatMap {
              case net @ DFNet.Assignment(dcl: DFVal.Dcl, from)
                  if assignCnt.getOrElse(dcl, 0) == 1 && net.getOwner == domainOwner =>
                net.collectRelMembers.filter(collectFilter) :+ net
              case _ => Nil
            }
            val singleAssignmentsSet = singleAssignments.toSet
            val processBlockAllMembers =
              combinationalMembers.filterNot(singleAssignmentsSet.contains)
            // println("singleAssignments:")
            // println(singleAssignments.mkString("\n"))
            // println("processBlockAllMembers:")
            // println(processBlockAllMembers.mkString("\n"))
            // println("----")
            val processAllDsn =
              new MetaDesign(updatedOwner, Patch.Add.Config.InsideLast, domainType = ED):
                // create a combinational process if needed
                val hasProcessAll = dclREGList.nonEmpty || processBlockAllMembers.exists {
                  case net: DFNet               => true
                  case ch: DFConditional.Header => true
                  case _                        => false
                }
                val dcl_din_vars =
                  for (orig <- dclREGList)
                    yield orig.asValAny.genNewVar(using
                      dfc.setMeta(orig.meta.setName(s"${orig.getName}_din"))
                    ).asIR
                if (hasProcessAll)
                  process(all) {
                    val inVHDL = co.backend.isVHDL
                    dclREGList.lazyZip(dcl_din_vars).foreach { (dclREG, dcl_din) =>
                      if (inVHDL) dcl_din.asVarAny :== dclREG.asValAny
                      else dcl_din.asVarAny := dclREG.asValAny
                    }
                    if (inVHDL)
                      plantMembers(
                        domainOwner,
                        processBlockAllMembers.view.map {
                          case net: DFNet => net.copy(op = DFNet.Op.NBAssignment)
                          case m          => m
                        }
                      )
                    else plantMembers(domainOwner, processBlockAllMembers)
                  }
                val dclChangeList = dclREGList.lazyZip(dcl_din_vars).toList
                // create map of all reg dcls references that are used to assign to the registers,
                // or partial selection of the registers
                val dclChangeRefMap = mutable.Map.empty[DFVal.Dcl, Set[DFRefAny]]
                processBlockAllMembers.foreach {
                  case net: DFNet =>
                    @tailrec def addDinRef(ref: DFRefAny): Unit =
                      ref.get match
                        case dcl: DFVal.Dcl if dcl.modifier.isReg =>
                          dclChangeRefMap += dcl -> (dclChangeRefMap.getOrElse(dcl, Set()) + ref)
                        case partial: DFVal.Alias.Partial =>
                          addDinRef(partial.relValRef)
                        case _ => // do nothing
                    addDinRef(net.lhsRef)
                  case _ => // do nothing
                }
                val dclChangePatch = dclChangeList.map((from, to) =>
                  val changeRefs = dclChangeRefMap.getOrElse(from, Set()).toSet
                  val refFilter = new Patch.Replace.RefFilter:
                    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
                      changeRefs
                  from -> Patch.Replace(
                    to,
                    Patch.Replace.Config.ChangeRefOnly,
                    refFilter
                  )
                )

            val processSeqDsn =
              new MetaDesign(updatedOwner, Patch.Add.Config.InsideLast, domainType = ED):
                lazy val clk = clkRstOpt.clkOpt.get.asValOf[DFOpaque[DFOpaque.Clk]]
                lazy val rst = clkRstOpt.rstOpt.get.asValOf[DFOpaque[DFOpaque.Rst]]

                import processAllDsn.dclChangeList

                def regInitBlock() =
                  regNets.foreach:
                    case rn if rn.initOption.nonEmpty && !rn.initOption.get.isBubble =>
                      rn.regVar.asVarAny :== rn.initOption.get.cloneAnonValueAndDepsHere.asValAny
                    case _ =>
                  dclREGList.foreach:
                    case dcl if dcl.initRefList.nonEmpty && !dcl.initRefList.head.get.isBubble =>
                      dcl.asVarAny :== dcl.initRefList.head.get.cloneAnonValueAndDepsHere.asValAny
                    case _ =>
                def regSaveBlock() =
                  regNets.foreach: rn =>
                    rn.regVar.asVarAny :== rn.relVal.asValAny
                  dclChangeList.foreach:
                    case (dclREG, dcl_din) =>
                      dclREG.asVarAny :== dcl_din.asValAny
                def ifRstActive =
                  val RstCfg.Explicit(_, active: RstCfg.Active, _) = rstCfg: @unchecked
                  val cond = active match
                    case RstCfg.Active.High => rst.actual == 1
                    case RstCfg.Active.Low  => rst.actual == 0
                  DFIf.singleBranch(Some(cond), DFIf.Header(dfhdl.core.DFUnit), regInitBlock)
                def ifRstActiveElseRegSaveBlock(): Unit =
                  val (_, rstBranch) = ifRstActive
                  DFIf.singleBranch(None, rstBranch, regSaveBlock)
                def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                  val ClkCfg.Explicit(edge, _, _) = clkCfg: @unchecked
                  val cond = edge match
                    case ClkCfg.Edge.Rising  => clk.actual.rising
                    case ClkCfg.Edge.Falling => clk.actual.falling
                  DFIf.singleBranch(
                    Some(cond),
                    ifRstOption.getOrElse(DFIf.Header(dfhdl.core.DFUnit)),
                    block
                  )

                if (clkCfg != None && (regNets.nonEmpty || dclREGList.nonEmpty))
                  val regAliasesHaveRst =
                    regNets.exists(rn => rn.initOption.nonEmpty && !rn.initOption.get.isBubble)
                  val dclREGsHaveRst =
                    dclREGList.exists(d =>
                      d.initRefList.nonEmpty && !d.initRefList.head.get.isBubble
                    )
                  if (rstCfg != None && (dclREGsHaveRst || regAliasesHaveRst))
                    val RstCfg.Explicit(mode: RstCfg.Mode, _, _) = rstCfg: @unchecked
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

                // adding single assignments at the bottom
                plantMembers(
                  domainOwner,
                  singleAssignments.view.map {
                    case net: DFNet => net.copy(op = DFNet.Op.Connection)
                    case m          => m
                  }
                )

            val movedMembersRemovalPatches = combinationalMembers.map { m =>
              m -> Patch.Remove(isMoved = true)
            }
            List(
              Some(ownerDomainPatch),
              Some(domainOwner -> Patch.Add(processAllDsn, Patch.Add.Config.InsideLast)),
              processAllDsn.dclChangePatch,
              Some(domainOwner -> Patch.Add(processSeqDsn, Patch.Add.Config.InsideLast)),
              movedMembersRemovalPatches,
              regAliasRemovalPatches
            ).flatten
          // other domains
          case _ => None
        end match
      // other owners
      case _ => None
    }
    val firstPart = designDB.patch(patchList)
    locally {
      import firstPart.getSet
      val patchList = firstPart.members.collect {
        case dcl: DFVal.Dcl if dcl.modifier.isReg =>
          val updatedDcl =
            dcl.copy(initRefList = Nil, modifier = dcl.modifier.copy(special = Modifier.Ordinary))
          dcl -> Patch.Replace(updatedDcl, Patch.Replace.Config.FullReplacement)

      }
      firstPart.patch(patchList)
    }
  end transform
end ToED

extension [T: HasDB](t: T) def toED(using CompilerOptions): DB = StageRunner.run(ToED)(t.db)
