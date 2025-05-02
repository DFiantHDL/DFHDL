package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Modifier
import dfhdl.core.{DFIf, DFOwnerAny, DFOpaque}
import dfhdl.core.DomainType.ED
import scala.annotation.tailrec
import scala.collection.mutable
case object ToED extends Stage:
  def dependencies: List[Stage] =
    List(DropUnreferencedAnons, ToRT, NameRegAliases, ExplicitNamedVars, AddClkRst,
      SimpleOrderMembers)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val domainAnalysis = new DomainAnalysis(designDB)
    // the last handled design to know when a design is switched to clear
    // the handledDesignDcls set (saving as top for initial since transforming bottom-up,
    // and this guarantees to work at any case and not required if we only have a single design top
    // with no hierarchies)
    var handledDesign: DFDesignBlock = designDB.top
    // save handled REG dcls for a given design at any domain level
    val handledDesignREGDclSet = mutable.Set.empty[DFVal.Dcl]
    val patchList: List[(DFMember, Patch)] = designDB.domainOwnerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (domainOwner: (DFDomainOwner & DFBlock), members) =>
        val design = domainOwner.getThisOrOwnerDesign
        // clear handledDesignREGDclSet on design change (to keep the set small, since no need
        // to remember these Dcls across designs)
        if (handledDesign != design)
          handledDesign = design
          handledDesignREGDclSet.clear()
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

            val assignCnt = mutable.Map.empty[DFVal.Dcl, Int]
            def anotherAssignCnt(toVal: DFVal): Unit =
              toVal.departialDcl.foreach {
                case (dcl: DFVal.Dcl, _) if !dcl.isReg =>
                  assignCnt.update(dcl, assignCnt.getOrElse(dcl, 0) + 1)
                case _ =>
              }
            def collectFilter(member: DFMember): Boolean = member match
              case _: DFVal.Dcl                               => false
              case _: DFVal.DesignParam                       => false
              case _: DFOwnerNamed                            => false
              case dfVal: DFVal if dfVal.isReferencedByAnyDcl => false
              case _                                          => true

            def getProcessAllMembers(list: List[DFMember]): List[DFMember] =
              val processBlockAllMembersSet: Set[DFMember] = list.view.flatMap {
                case net @ DFNet.Assignment(toVal, _) =>
                  anotherAssignCnt(toVal)
                  net :: net.collectRelMembers
                case ch: DFConditional.Header if ch.dfType == DFUnit =>
                  ch.collectRelMembers(true)
                case cb: DFConditional.Block =>
                  cb.guardRef.get match
                    case dfVal: DFVal => cb :: dfVal.collectRelMembers(false)
                    case _            => List(cb)
                case textOut: TextOut =>
                  textOut :: textOut.collectRelMembers
                case _ => None
              }.toSet

              list.view.filter(collectFilter).flatMap {
                case m if processBlockAllMembersSet.contains(m) => Some(m)
                case _                                          => None
              }.toList
            end getProcessAllMembers
            val combinationalMembers = getProcessAllMembers(members)
            val singleAssignments = combinationalMembers.flatMap {
              case net @ DFNet.Assignment(dcl: DFVal.Dcl, from)
                  if !dcl.isReg &&
                    assignCnt.getOrElse(dcl, 0) == 1 && net.getOwner == domainOwner =>
                net.collectRelMembers.filter(collectFilter) :+ net
              case _ => Nil
            }.distinct
            val singleAssignmentsSet = singleAssignments.toSet
            val processBlockAllMembers =
              combinationalMembers.filterNot(singleAssignmentsSet.contains)
            // saving REG Dcls that were not previous handled in the design (possibly) in another internal domains.
            // we use a linked set for order consistency
            val dclREGSet = mutable.LinkedHashSet.empty[DFVal.Dcl]
            members.foreach {
              case dcl: DFVal.Dcl if dcl.isReg && !handledDesignREGDclSet.contains(dcl) =>
                dclREGSet += dcl
              case _ =>
            }
            // save REG Dcls that require a default assignment to their DIN variable
            // (if they eventually require a DIN variable)
            // currently, if guarded by a conditional, then we assume it is required.
            // also, if it's partially assigned even once, then we assume it is required.
            // TODO: it is possible to check for complete coverage test of assignment, to remove redundant
            // default assignments in the future.
            val dclREGRequiresDefaultSet = mutable.Set.empty[DFVal.Dcl]
            // domain is purely sequential if the configuration is not combinational and
            // all the remaining non-single assignment variables are either REGs or SHARED variables
            var domainIsPureSequential = clkCfg != None // clock configuration -> not combinational
            processBlockAllMembers.foreach {
              case net @ DFNet.Assignment(dfVal: DFVal, _) =>
                val (dcl, range) = dfVal.departialDcl.get
                if (dcl.isReg)
                  // it could be that we are assigning to a Dcl outside the domain. this is fine,
                  // as long as we mark it as handled. two different domains are guaranteed not to assign
                  // to the same dcl.
                  dclREGSet += dcl
                  handledDesignREGDclSet += dcl
                net.getOwnerBlock match
                  // simple test: if guarded by a conditional -> requires a default
                  case _: DFConditional.Block =>
                    dclREGRequiresDefaultSet += dcl
                  // simple test: partially assigned even once -> requires a default
                  case _ if range.length != dcl.width =>
                    dclREGRequiresDefaultSet += dcl
                  case _ => // do nothing
                if (!dcl.isReg && !dcl.modifier.isShared)
                  domainIsPureSequential = false
              case x =>
            }
            // the full list of handled REG Dcls in this domain
            val dclREGList = dclREGSet.toList
            // println("singleAssignments:")
            // println(singleAssignments.mkString("\n"))
            // println("processBlockAllMembers:")
            // println(processBlockAllMembers.mkString("\n"))
            // println("----")
            val processAllDsn =
              new MetaDesign(domainOwner, Patch.Add.Config.InsideLast, domainType = ED):
                // variables to transfer combinational information from the combinational block
                // to the sequential block, to be registered
                val dcl_din_vars = dclREGList.map: orig =>
                  if (domainIsPureSequential) None
                  else
                    Some(
                      orig.asValAny.genNewVar(using
                        dfc.setMeta(orig.meta.setName(s"${orig.getName}_din"))
                      ).asIR
                    )
                val dclChangeList = dclREGList.lazyZip(dcl_din_vars).collect {
                  case (dclREG, Some(dcl_din)) => (dclREG, dcl_din)
                }.toList
                // create a combinational process if needed
                val hasProcessAll =
                  !domainIsPureSequential && (dclChangeList.nonEmpty || processBlockAllMembers.exists {
                    case net: DFNet               => true
                    case ch: DFConditional.Header => true
                    case _                        => false
                  })
                if (hasProcessAll)
                  process(all) {
                    val inVHDL = co.backend.isVHDL
                    dclChangeList.foreach {
                      case (dclREG, dcl_din) if dclREGRequiresDefaultSet.contains(dclREG) =>
                        if (inVHDL) dcl_din.asVarAny :== dclREG.asValAny
                        else dcl_din.asVarAny := dclREG.asValAny
                      case _ => // do nothing
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
                // create map of all reg dcls references that are used to assign to the registers,
                // or partial selection of the registers
                val dclChangeRefMap = mutable.Map.empty[DFVal.Dcl, Set[DFRefAny]]
                processBlockAllMembers.foreach {
                  case net: DFNet =>
                    @tailrec def addDinRef(ref: DFRefAny): Unit =
                      ref.get match
                        case dcl: DFVal.Dcl if dcl.isReg =>
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
              new MetaDesign(domainOwner, Patch.Add.Config.InsideLast, domainType = ED):
                lazy val clk = clkRstOpt.clkOpt.get.asValOf[DFOpaque[DFOpaque.Clk]]
                lazy val rst = clkRstOpt.rstOpt.get.asValOf[DFOpaque[DFOpaque.Rst]]

                import processAllDsn.dclChangeList

                def regInitBlock() = dclREGList.foreach:
                  case dcl if dcl.hasNonBubbleInit =>
                    dcl.asVarAny :== dcl.initRefList.head.get.cloneAnonValueAndDepsHere.asValAny
                  case _ =>
                def regSaveBlock() =
                  if (domainIsPureSequential)
                    plantMembers(
                      domainOwner,
                      processBlockAllMembers.view.map {
                        case net @ DFNet.Assignment(toVal, _)
                            if !toVal.departialDcl.get._1.modifier.isShared =>
                          net.copy(op = DFNet.Op.NBAssignment)
                        case m => m
                      }
                    )
                  else
                    dclChangeList.foreach: (dclREG, dcl_din) =>
                      dclREG.asVarAny :== dcl_din.asValAny
                def ifRstActive =
                  val RstCfg.Explicit(active = active) = rstCfg: @unchecked
                  val cond = active match
                    case RstCfg.Active.High => rst.actual == 1
                    case RstCfg.Active.Low  => rst.actual == 0
                  DFIf.singleBranch(Some(cond), DFIf.Header(dfhdl.core.DFUnit), regInitBlock)
                def ifRstActiveElseRegSaveBlock(): Unit =
                  val (_, rstBranch) = ifRstActive
                  DFIf.singleBranch(None, rstBranch, regSaveBlock)
                def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                  val ClkCfg.Explicit(edge = edge) = clkCfg: @unchecked
                  val cond = edge match
                    case ClkCfg.Edge.Rising  => clk.actual.rising
                    case ClkCfg.Edge.Falling => clk.actual.falling
                  DFIf.singleBranch(
                    Some(cond),
                    ifRstOption.getOrElse(DFIf.Header(dfhdl.core.DFUnit)),
                    block
                  )
                val hasSeqProcess =
                  clkCfg != None && (dclREGList.nonEmpty || processBlockAllMembers.nonEmpty && domainIsPureSequential)

                if (hasSeqProcess)
                  if (rstCfg != None)
                    val RstCfg.Explicit(mode = mode) = rstCfg: @unchecked
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
              Some(domainOwner -> Patch.Add(processAllDsn, Patch.Add.Config.InsideLast)),
              processAllDsn.dclChangePatch,
              Some(domainOwner -> Patch.Add(processSeqDsn, Patch.Add.Config.InsideLast)),
              movedMembersRemovalPatches
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
        case dcl: DFVal.Dcl if dcl.isReg =>
          // if the domain has no reset, then the register init is preserved for the signal
          // as a startup reset value
          val updatedInitRefList = dcl.getDomainType match
            case DomainType.RT(RTDomainCfg.Explicit(rstCfg = None)) =>
              dcl.initRefList
            case _ => Nil
          val updatedDcl =
            dcl.copy(
              initRefList = updatedInitRefList,
              modifier = dcl.modifier.copy(special = Modifier.Ordinary)
            )
          dcl -> Patch.Replace(updatedDcl, Patch.Replace.Config.FullReplacement)
        case domainOwner: DFDomainOwner if domainOwner.domainType != DomainType.ED =>
          // changing the owner from RT domain to ED domain
          val updatedOwner = domainOwner match
            case design: DFDesignBlock => design.copy(domainType = DomainType.ED)
            case domain: DomainBlock   => domain.copy(domainType = DomainType.ED)
            case ifc: DFInterfaceOwner => ifc.copy(domainType = DomainType.ED)
          domainOwner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)
      }
      firstPart.patch(patchList)
    }
  end transform
end ToED

extension [T: HasDB](t: T) def toED(using CompilerOptions): DB = StageRunner.run(ToED)(t.db)
