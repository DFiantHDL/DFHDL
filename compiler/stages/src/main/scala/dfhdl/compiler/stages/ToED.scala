package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.History.Op as HistoryOp
import DFVal.Modifier
import dfhdl.core.{DFC, DFIf, DFOwnerAny}
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
        domainOwner.domainType match
          // only care about register-transfer domains.
          // those have wires and regs that we need to simplify.
          case domainType @ DomainType.RT(cfg @ RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
            val clkRstOpt = domainAnalysis.designDomains((domainOwner.getThisOrOwnerDesign, cfg))

            val dclREGList = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier.reg => dcl
            }

            // changing the owner from RT domain to ED domain
            val updatedOwner = domainOwner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.ED)
              case domain: DomainBlock   => domain.copy(domainType = DomainType.ED)
            val ownerDomainPatch =
              domainOwner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            val regNets = designDB.designMemberTable(domainOwner.getThisOrOwnerDesign).collect:
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
            @tailrec def getDeps(
                leftMembers: List[DFMember],
                handledMembers: Set[DFMember]
            ): Set[DFMember] =
              leftMembers match
                case head :: last =>
                  head match
                    case dcl: DFVal.Dcl => getDeps(last, handledMembers)
                    case _ if !handledMembers.contains(head) =>
                      val moreMembers = head.getRefs.view.flatMap {
                        case _: DFRef.TypeRef => None
                        case r                => Some(r.get)
                      }
                      getDeps(last ++ moreMembers, handledMembers + head)
                    case _ => getDeps(last, handledMembers)
                case Nil => handledMembers

            def processMembers(list: List[DFMember]): List[DFMember] =
              val processBlockAllMembersSet = list.view.flatMap {
                case DesignParam(_)                 => None
                case net: DFNet if net.isConnection => None
                case net @ DFNet.Assignment(_, fromVal) if removedNets.contains(net) =>
                  getDeps(List(fromVal), Set())
                case net: DFNet =>
                  getDeps(List(net), Set())
                case ch: DFConditional.Header if ch.dfType == DFUnit =>
                  getDeps(List(ch), Set())
                case _ => None
              }.toSet

              list.flatMap {
                case dcl: DFVal.Dcl => None
                case DesignParam(_) => None
                case cb: DFConditional.Block if cb.getHeaderCB.dfType == DFUnit =>
                  cb :: processMembers(designDB.blockMemberTable(cb))
                case dsn: DFOwnerNamed                          => None
                case history: DFVal.Alias.History               => None
                case m if processBlockAllMembersSet.contains(m) => Some(m)
                case _                                          => None
              }
            end processMembers
            val processBlockAllMembers = processMembers(members)
            // println(processBlockAllMembers.mkString("\n"))

            val processAllDsn =
              new MetaDesign(updatedOwner, Patch.Add.Config.InsideLast, domainType = DFC.Domain.ED):
                // create a combinational process if needed
                val hasProcessAll = dclREGList.nonEmpty || processBlockAllMembers.exists {
                  case net: DFNet => true
                  case _          => false
                }
                val dcl_din_vars =
                  for (orig <- dclREGList)
                    yield orig.asValAny.genNewVar(using
                      dfc.setMeta(orig.meta.setName(s"${orig.getName}_din"))
                    ).asIR
                val dclChangeList = dclREGList.lazyZip(dcl_din_vars).toList
                val newRefs = mutable.Map.empty[DFVal.Dcl, Set[DFRefAny]]
                if (hasProcessAll)
                  process(all) {
                    val inVHDL = co.backend match
                      case _: dfhdl.backends.vhdl => true
                      case _                      => false
                    dclREGList.lazyZip(dcl_din_vars).foreach { (dclREG, dcl_din) =>
                      if (inVHDL) dcl_din.asVarAny :== dclREG.asValAny
                      else dcl_din.asVarAny := dclREG.asValAny
                    }
                    processBlockAllMembers.foreach {
                      case net: DFNet =>
                        @tailrec def addDinRef(ref: DFRefAny): Unit =
                          ref.get match
                            case dcl: DFVal.Dcl if dcl.modifier.reg =>
                              newRefs += dcl -> (newRefs.getOrElse(dcl, Set()) + ref)
                            case partial: DFVal.Alias.Partial =>
                              addDinRef(partial.relValRef)
                            case _ => // do nothing
                        addDinRef(net.lhsRef)
                        if (inVHDL) plantMember(net.copy(op = DFNet.Op.NBAssignment))
                        else plantMember(net)
                      case m =>
                        plantMember(m)
                    }
                  }
                val dclChangePatch = dclChangeList.map((from, to) =>
                  val refFilter = new Patch.Replace.RefFilter:
                    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
                      newRefs(from).toSet
                  from -> Patch.Replace(
                    to,
                    Patch.Replace.Config.ChangeRefOnly,
                    refFilter
                  )
                )

            val processSeqDsn =
              new MetaDesign(updatedOwner, Patch.Add.Config.InsideLast, domainType = DFC.Domain.ED):
                lazy val clk = clkRstOpt.clkOpt.get.asValOf[Bit]
                lazy val rst = clkRstOpt.rstOpt.get.asValOf[Bit]

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

                if (clkCfg != None && (regNets.nonEmpty || dclREGList.nonEmpty))
                  val regAliasesHaveRst =
                    regNets.exists(rn => rn.initOption.nonEmpty && !rn.initOption.get.isBubble)
                  val dclREGsHaveRst =
                    dclREGList.exists(d =>
                      d.initRefList.nonEmpty && !d.initRefList.head.get.isBubble
                    )
                  if (rstCfg != None && (dclREGsHaveRst || regAliasesHaveRst))
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

            val movedMembersRemovalPatches = processBlockAllMembers.map { m =>
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
        case dcl: DFVal.Dcl if dcl.modifier.reg =>
          val updatedDcl =
            dcl.copy(initRefList = Nil, modifier = dcl.modifier.copy(reg = false))
          dcl -> Patch.Replace(updatedDcl, Patch.Replace.Config.FullReplacement)

      }
      firstPart.patch(patchList)
    }
  end transform
end ToED

extension [T: HasDB](t: T) def toED(using CompilerOptions): DB = StageRunner.run(ToED)(t.db)
