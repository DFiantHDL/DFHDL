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

/** Lowers RT domains to ED domains.
  *
  * Reads the resolved `@timing.clock` / `@timing.reset` / `@timing.related` annotations from each
  * RT domain owner's meta (written by [[ExplicitClkRstCfg]] and preserved by [[AddClkRst]]) to:
  *
  *   - Pick the clock-edge condition for sequential `process(clk)` / `process(clk, rst)`.
  *   - Pick the reset mode (sync vs async) and active polarity for the reset branch.
  *   - Walk `@timing.related(ref)` to the related target's clk/rst Dcls.
  *
  * Strips the resolved timing annotations on RT→ED conversion — by that point the configuration is
  * fully baked into the generated `Clk_<grp>` / `Rst_<grp>` opaque port types and the annotations
  * are redundant.
  */
case object ToED extends HierarchyStage:
  def dependencies: List[Stage] =
    List(
      DropUnreferencedAnons, ToRT, DropRTProcess, NameRegAliases, ExplicitNamedVars,
      ExplicitCondExprAssign, SplitInitialBlocks, DropInitialBlocks, AddClkRst,
      SimpleOrderMembers
    )
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  // Only a DYNAMIC domain lowers to ED. The test is POSITIVE on purpose: its former `!= ED`
  // form also matched the STATIC domain, and a static function's def design must keep it. Static
  // is not a timing model to lower, and the printers key on it (VHDL `pure function`, and formals
  // taken from design parameters rather than from input ports, which it has none of).
  private def lowersToED(domainType: DomainType): Boolean = domainType match
    case DomainType.DF | DomainType.RT     => true
    case DomainType.ED | DomainType.Static => false
  // ToED is per-design: every domain owner it transforms, and the clk/rst Dcls
  // it reads, live in the current sub-DB (the `subDB` helper).
  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      rg: RefGen
  ): DB =

    // Annotation-based mirror of `DomainAnalysis.designDomains`. For an RT owner returns
    // its (clkOpt, rstOpt) Dcl pair; if the owner carries `@timing.related(ref)`, the
    // pair is taken from the related target (transitively).
    val ownerClkRstCache =
      mutable.Map.empty[DFDomainOwner, (Option[DFVal.Dcl], Option[DFVal.Dcl])]
    def relatedTarget(owner: DFDomainOwner): Option[DFDomainOwner] =
      owner.meta.annotations.collectFirst {
        case rel: constraints.Timing.Related => rel.ref.get
      }
    def lookupClkRst(owner: DFDomainOwner): (Option[DFVal.Dcl], Option[DFVal.Dcl]) =
      ownerClkRstCache.getOrElseUpdate(
        owner,
        relatedTarget(owner) match
          case Some(target) => lookupClkRst(target)
          case None         =>
            val members = subDB.domainOwnerMemberTable(owner)
            val clkOpt = members.collectFirst {
              case clk: DFVal.Dcl if clk.isClkDcl => clk
            }
            val rstOpt = members.collectFirst {
              case rst: DFVal.Dcl if rst.isRstDcl => rst
            }
            (clkOpt, rstOpt)
      )

    // the last handled design to know when a design is switched to clear
    // the handledDesignDcls set (saving as top for initial since transforming bottom-up,
    // and this guarantees to work at any case and not required if we only have a single design top
    // with no hierarchies)
    var handledDesign: DFDesignBlock = subDB.top
    // save handled REG dcls for a given design at any domain level
    val handledDesignREGDclSet = mutable.Set.empty[DFVal.Dcl]
    val patchList: List[(DFMember, Patch)] = subDB.domainOwnerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (domainOwner: (DFDomainOwner & DFBlock), members) =>
        val design = domainOwner.getThisOrOwnerDesign
        // clear handledDesignREGDclSet on design change (to keep the set small, since no need
        // to remember these Dcls across designs)
        if (handledDesign != design)
          handledDesign = design
          handledDesignREGDclSet.clear()
        domainOwner.domainType match
          case DomainType.RT =>
            // Resolve the effective clk/rst annotations: walk through any `@timing.related`
            // chain to the originating owner whose meta carries the actual clk/rst annotations.
            @tailrec def resolveTimingOwner(o: DFDomainOwner): DFDomainOwner =
              relatedTarget(o) match
                case Some(t) => resolveTimingOwner(t)
                case None    => o
            val timingOwner = resolveTimingOwner(domainOwner)
            val clkAnnotOpt: Option[constraints.Timing.Clock] =
              timingOwner.meta.annotations.collectFirst {
                case c: constraints.Timing.Clock => c
              }
            // The reset is resolved through the `@timing.related` chain honoring `includeReset`:
            // a related link that excludes the reset yields None here, so no reset branch is
            // generated and the registers keep their init values (see the second-pass patch).
            val rstAnnotOpt: Option[constraints.Timing.Reset] =
              domainOwner.resolvedRstAnnot
            // Note: a purely combinational RT owner has no clk/rst annotations after relaxation;
            // we still process it (the original ToED falls into the same branch via `Config(cfg)`
            // even when `clkCfg = None && rstCfg = None`) so single-assignment promotion to
            // connections and `process(all)` wrapping still happen.
            val clkRstOpt = lookupClkRst(domainOwner)

            // Initial blocks and their contents are excluded from the combinational/sequential
            // accounting below (`domainOwnerMemberList` groups members by owner *domain*, so an
            // initial block's inner members appear in `members` too). When the domain has a
            // reset and a sequential process, the (post-SplitInitialBlocks, non-converted)
            // initial blocks are planted into the reset branch with non-blocking assignments;
            // otherwise they stay as-is and exit ToED as ED initial blocks.
            val initialPBs = members.collect { case pb: ProcessBlock if pb.isInitial => pb }
            val initialMemberSet: Set[DFMember] =
              initialPBs.view.flatMap(pb => pb :: pb.members(MemberView.Flattened)).toSet
            val nonInitialMembers =
              if (initialPBs.isEmpty) members else members.filterNot(initialMemberSet)

            val assignCnt = mutable.Map.empty[DFVal.Dcl, Int]
            def anotherAssignCnt(toVal: DFVal): Unit =
              toVal.departialDcl.foreach {
                case (dcl: DFVal.Dcl, _) if !dcl.isReg =>
                  assignCnt.update(dcl, assignCnt.getOrElse(dcl, 0) + 1)
                case _ =>
              }
            def collectFilter(member: DFMember): Boolean = member match
              case IteratorDcl()        => true
              case _: DFVal.Dcl         => false
              case _: DFVal.DesignParam => false
              case DclConst()           => false
              case _: DFOwnerNamed      => false
              // a DIN read marker is replaced outright (see `dinReadPatches`), so it must not also
              // be moved into the generated process: the two patches would collide on it
              case _: DFVal.Alias.RegDIN                              => false
              case dfVal: DFVal if dfVal.isReferencedByAnyDclOrDesign => false
              case _                                                  => true

            def getProcessAllMembers(list: List[DFMember]): List[DFMember] =
              val processBlockAllMembersSet: Set[DFMember] = list.view.flatMap {
                case net @ DFNet.Assignment(toVal, _) =>
                  anotherAssignCnt(toVal)
                  net :: net.collectRelMembers
                case ch: DFConditional.Header if ch.dfType == DFUnit =>
                  ch.collectRelMembers(true)
                case loop: DFLoop.DFForBlock =>
                  val range = loop.rangeRef.get
                  loop :: loop.iteratorRef.get :: range ::
                    Iterator(range.startRef, range.endRef, range.stepRef)
                      .flatMap(_.get.collectRelMembers(true)).toList
                case cb: (DFConditional.Block | DFLoop.Block | TextOut) =>
                  cb :: cb.getRefs.view.filterNot(_.isTypeRef).map(_.get).flatMap {
                    case dfVal: DFVal => dfVal.collectRelMembers(true)
                    case _            => Nil
                  }.toList
                case _ => None
              }.toSet

              list.view.filter(collectFilter).flatMap {
                case m if processBlockAllMembersSet.contains(m) => Some(m)
                case _                                          => None
              }.toList
            end getProcessAllMembers
            val combinationalMembers = getProcessAllMembers(nonInitialMembers)
            // A DIN read yields the register's pending value AT THE POSITION OF THE READ, so the
            // statement holding it must stay in the process body, in order. Promoting it to a
            // concurrent connection would instead read the shadow variable's final value, and when
            // the same statement also feeds that shadow it closes a combinational loop:
            // {{{
            // always_comb begin r_din = r; r_din = sum; end
            // assign sum = r_din + 8'd1;   // sum depends on r_din depends on sum
            // }}}
            def readsDIN(net: DFNet): Boolean = net.collectRelMembers.exists {
              case _: DFVal.Alias.RegDIN => true
              case _                     => false
            }
            val singleAssignments = combinationalMembers.flatMap {
              case net @ DFNet.Assignment(dcl: DFVal.Dcl, from)
                  if !dcl.isReg && !readsDIN(net) &&
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
            var domainIsPureSequential = clkAnnotOpt.isDefined
            processBlockAllMembers.foreach {
              case net @ DFNet.Assignment(dfVal: DFVal, _) =>
                val (dcl, slice) = dfVal.departialDcl.get
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
                  // conservative test: partially assigned (or can't be proven full) -> requires a default
                  case _ if slice.isFullOf(dcl.dfType.widthIntOpt) != Tri.Yes =>
                    dclREGRequiresDefaultSet += dcl
                  case _ => // do nothing
                if (!dcl.isReg && !dcl.modifier.isShared)
                  domainIsPureSequential = false
              case x =>
            }
            // ==== register DIN reads ====
            // `r.din` read as a value is the register's pending next-cycle value, which is exactly
            // what the `<reg>_din` shadow variable below holds. Such a read therefore needs the
            // shadow variable to exist (so the domain cannot stay purely sequential) and to be
            // seeded with the register's own value, so that a read taken before any assignment in
            // the cycle body yields the register. The reads themselves are resolved to the variable
            // further down, together with the assignment redirection that already happens there.
            val dinReadAliases = nonInitialMembers.collect { case a: DFVal.Alias.RegDIN => a }
            val dinReadREGs =
              dinReadAliases.flatMap(_.relValRef.get.departialDcl.map(_._1)).distinct
            dinReadREGs.foreach { dcl =>
              // mirrors the assignment path above: the DIN may belong to a Dcl outside this domain,
              // and marking it handled keeps another domain from claiming it too
              dclREGSet += dcl
              handledDesignREGDclSet += dcl
              dclREGRequiresDefaultSet += dcl
            }
            if (dinReadAliases.nonEmpty) domainIsPureSequential = false
            // an assignment whose target resolves to a DIN-read register's shadow (used by the
            // VHDL process-variable form below, where such an assignment must stay blocking)
            def assignsDinReadShadow(net: DFNet): Boolean =
              net.lhsRef.get.departialDcl.exists((dcl, _) => dinReadREGs.contains(dcl))
            // the full list of handled REG Dcls in this domain
            val dclREGList = dclREGSet.toList
            val hasSeqProcess =
              clkAnnotOpt.isDefined &&
                (dclREGList.nonEmpty ||
                  processBlockAllMembers.nonEmpty && domainIsPureSequential)
            // initial blocks are planted into the reset branch (see `regInitBlock`) exactly
            // when a sequential process with a reset is generated; only then are they removed
            val plantInitialPBs = hasSeqProcess && rstAnnotOpt.isDefined && initialPBs.nonEmpty
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
                  !domainIsPureSequential &&
                    (dclChangeList.nonEmpty || processBlockAllMembers.exists {
                      case net: DFNet               => true
                      case ch: DFConditional.Header => true
                      case _                        => false
                    })
                // VHDL only, and only for a shadow that is READ: under signal assignment every RHS
                // sees the pre-process value, so a read-modify-write chain such as `r_din <= r_din
                // + 1` twice increments once, and being self-referential inside a `process(all)` it
                // never settles. Such a shadow therefore becomes a process VARIABLE with blocking
                // assignments, published to the design-level signal as the last statement of the
                // process. The signal is what the clocked process and any concurrent reader see.
                // Verilog needs none of this: its shadow is already assigned blocking in
                // `always_comb`. Keyed per register, so a register without DIN reads is untouched.
                val dinLocalVars = mutable.Map.empty[DFVal.Dcl, DFVal]
                if (hasProcessAll)
                  process(all) {
                    val inVHDL = co.backend.isVHDL
                    if (inVHDL)
                      dclChangeList.foreach { (dclREG, dcl_din) =>
                        if (dinReadREGs.contains(dclREG))
                          dinLocalVars += dclREG -> dcl_din.asValAny.genNewVar(using
                            dfc.setMeta(dcl_din.meta.setName(s"${dcl_din.getName}_v"))
                          ).asIR
                      }
                    dclChangeList.foreach {
                      case (dclREG, dcl_din) if dclREGRequiresDefaultSet.contains(dclREG) =>
                        dinLocalVars.get(dclREG) match
                          case Some(local)    => local.asVarAny := dclREG.asValAny
                          case None if inVHDL => dcl_din.asVarAny :== dclREG.asValAny
                          case None           => dcl_din.asVarAny := dclREG.asValAny
                      case _ => // do nothing
                    }
                    if (inVHDL)
                      plantMembers(
                        domainOwner,
                        processBlockAllMembers.view.map {
                          // an assignment into a DIN-read shadow keeps its blocking form: the
                          // shadow is a process variable, not a signal
                          case net: DFNet if assignsDinReadShadow(net) => net
                          case net: DFNet => net.copy(op = DFNet.Op.NBAssignment)
                          case m          => m
                        }
                      )
                    else plantMembers(domainOwner, processBlockAllMembers)
                    // publish the process variables to their design-level signals
                    dclChangeList.foreach { (dclREG, dcl_din) =>
                      dinLocalVars.get(dclREG).foreach { local =>
                        dcl_din.asVarAny :== local.asValAny
                      }
                    }
                  }
                // create map of all reg dcls references that are used to assign to the registers,
                // or partial selection of the registers
                val dclChangeRefMap = mutable.Map.empty[DFVal.Dcl, Set[DFRefAny]]
                @tailrec def addDinRef(ref: DFRefAny): Unit =
                  ref.get match
                    case dcl: DFVal.Dcl if dcl.isReg =>
                      dclChangeRefMap += dcl -> (dclChangeRefMap.getOrElse(dcl, Set()) + ref)
                    case partial: DFVal.Alias.Partial =>
                      addDinRef(partial.relValRef)
                    case _ => // do nothing
                processBlockAllMembers.foreach {
                  case net: DFNet => addDinRef(net.lhsRef)
                  case _          => // do nothing
                }
                // A partial DIN read (`r(5, 0).din`) keeps its existing selection chain and only
                // needs that chain re-rooted at the shadow variable, which is the very same
                // redirection an assignment LHS gets. A whole-value read has no chain to re-root
                // and is swapped for the variable directly (see `dinReadPatches`).
                dinReadAliases.foreach { alias =>
                  alias.relValRef.get match
                    case dcl: DFVal.Dcl if dcl.isReg => // no chain
                    case _                           => addDinRef(alias.relValRef)
                }
                val dclChangePatch = dclChangeList.map((from, to) =>
                  val changeRefs = dclChangeRefMap.getOrElse(from, Set()).toSet
                  val refFilter = new Patch.Replace.RefFilter:
                    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
                      changeRefs
                  // every redirected ref (assignment LHS and DIN read alike) originates from a
                  // member that lands inside the combinational process, so under the VHDL
                  // process-variable form they all resolve to the local rather than the signal
                  from -> Patch.Replace(
                    dinLocalVars.getOrElse(from, to),
                    Patch.Replace.Config.ChangeRefOnly,
                    refFilter
                  )
                )
                // the DIN read marker itself is dropped: a whole-value read becomes the shadow
                // variable, a partial read becomes its (now re-rooted) selection chain
                private val dinVarMap = dclChangeList.toMap
                val dinReadPatches = dinReadAliases.flatMap { alias =>
                  val target = alias.relValRef.get match
                    case dcl: DFVal.Dcl if dcl.isReg =>
                      dinLocalVars.get(dcl).orElse(dinVarMap.get(dcl))
                    case relVal => Some(relVal)
                  target.map(t =>
                    alias -> Patch.Replace(t, Patch.Replace.Config.ChangeRefAndRemove)
                  )
                }

            val processSeqDsn =
              new MetaDesign(domainOwner, Patch.Add.Config.InsideLast, domainType = ED):
                lazy val clk = clkRstOpt._1.get.asValOf[DFOpaque[DFOpaque.Clk]]
                lazy val rst = clkRstOpt._2.get.asValOf[DFOpaque[DFOpaque.Rst]]

                import processAllDsn.dclChangeList

                def regInitBlock() =
                  dclREGList.foreach:
                    case dcl if dcl.hasNonBubbleInit =>
                      dcl.asVarAny :== dcl.initRefList.head.get.cloneAnonValueAndDepsHere.asValAny
                    case _ =>
                  // plant the initial blocks' contents (cloned, with assignments converted to
                  // non-blocking) after the reg inits — RT initial content is const-RHS only,
                  // so the blocking→non-blocking conversion cannot change read semantics
                  initialPBs.foreach { pb =>
                    val contents = pb.members(MemberView.Flattened).map {
                      case net: DFNet => net.copy(op = DFNet.Op.NBAssignment)
                      case m          => m
                    }
                    plantClonedMembers(pb, contents)
                  }
                end regInitBlock
                def regSaveBlock() =
                  if (domainIsPureSequential)
                    plantMembers(
                      domainOwner,
                      processBlockAllMembers.view.map {
                        // shared-variable writes convert like the rest: inside the clocked
                        // process every write commits at the step's end
                        // (`SanityCheck.sharedAssignCheck` rejects a blocking shared write
                        // here), and the backends render the non-blocking net per the target's
                        // object class
                        case net @ DFNet.Assignment(_, _) =>
                          net.copy(op = DFNet.Op.NBAssignment)
                        case m => m
                      }
                    )
                  else
                    dclChangeList.foreach: (dclREG, dcl_din) =>
                      dclREG.asVarAny :== dcl_din.asValAny
                def ifRstActive =
                  val active = rstAnnotOpt.get.active.get
                  val cond = active match
                    case RstCfg.Active.High => rst.actual == 1
                    case RstCfg.Active.Low  => rst.actual == 0
                  DFIf.singleBranch(Some(cond), DFIf.Header(dfhdl.core.DFUnit), regInitBlock)
                def ifRstActiveElseRegSaveBlock(): Unit =
                  val (_, rstBranch) = ifRstActive
                  DFIf.singleBranch(None, rstBranch, regSaveBlock)
                def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                  val edge = clkAnnotOpt.get.edge.get
                  val cond = edge match
                    case ClkCfg.Edge.Rising  => clk.actual.rising
                    case ClkCfg.Edge.Falling => clk.actual.falling
                  DFIf.singleBranch(
                    Some(cond),
                    ifRstOption.getOrElse(DFIf.Header(dfhdl.core.DFUnit)),
                    block
                  )
                if (hasSeqProcess)
                  if (rstAnnotOpt.isDefined)
                    val mode = rstAnnotOpt.get.mode.get
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
            // initial blocks whose contents were planted (cloned) into the reset branch
            // are removed along with all their members
            val initialRemovalPatches =
              if (plantInitialPBs)
                initialPBs.flatMap { pb =>
                  (pb :: pb.members(MemberView.Flattened)).map(_ -> Patch.Remove())
                }
              else Nil
            List(
              Some(domainOwner -> Patch.Add(processAllDsn, Patch.Add.Config.InsideLast)),
              processAllDsn.dclChangePatch,
              processAllDsn.dinReadPatches,
              Some(domainOwner -> Patch.Add(processSeqDsn, Patch.Add.Config.InsideLast)),
              movedMembersRemovalPatches,
              initialRemovalPatches
            ).flatten
          // other domains
          case _ => None
        end match
    }
    val firstPart = subDB.patch(patchList)
    locally {
      import firstPart.getSet
      // Walk the `@timing.related` chain from a domain owner to the originating owner
      // whose meta carries the actual resolved clk/rst annotations.
      @tailrec def resolveTimingOwner(o: DFDomainOwner): DFDomainOwner =
        o.meta.annotations.collectFirst {
          case rel: constraints.Timing.Related => rel.ref.get
        } match
          case Some(t) => resolveTimingOwner(t)
          case None    => o
      val patchList = firstPart.members.collect {
        case dcl: DFVal.Dcl if dcl.isReg =>
          // if the domain has no reset, then the register init is preserved for the signal
          // as a startup reset value. The annotation channel encodes "no reset" as
          // an owner that has `@timing.clock` but no `@timing.reset` (the user-explicit
          // no-reset opt-out implemented in `getResolvedClkRst`). A `@timing.related` link
          // with `includeReset = false` is the other "no reset" source: the clock is still
          // resolved through the chain, but `resolvedRstAnnot` severs the reset, so the init
          // is likewise kept.
          val ownerDomain = dcl.getOwnerDomain
          val timingOwner = resolveTimingOwner(ownerDomain)
          val hasClkAnnot =
            timingOwner.meta.annotations.exists {
              case _: constraints.Timing.Clock => true; case _ => false
            }
          val hasRstAnnot = ownerDomain.resolvedRstAnnot.isDefined
          val updatedInitRefList =
            if (hasClkAnnot && !hasRstAnnot) dcl.initRefList else Nil
          val updatedDcl =
            dcl.copy(
              initRefList = updatedInitRefList,
              modifier = dcl.modifier.copy(special = Modifier.Ordinary)
            )
          dcl -> Patch.Replace(updatedDcl, Patch.Replace.Config.FullReplacement)
        case domainOwner: DFDomainOwner if lowersToED(domainOwner.domainType) =>
          // changing the owner from RT domain to ED domain. Strip all timing annotations
          // from the owner's meta — by this point the clk/rst configuration is fully baked
          // into the generated Clk_<grp>/Rst_<grp> opaque types and ports, so the
          // @timing.clock / @timing.reset / @timing.related annotations are redundant.
          def stripTimingAnnotations(meta: Meta): Meta =
            meta.copy(annotations = meta.annotations.filter {
              case _: constraints.Timing.Clock   => false
              case _: constraints.Timing.Reset   => false
              case _: constraints.Timing.Related => false
              case _                             => true
            })
          val updatedOwner = domainOwner match
            case design: DFDesignBlock =>
              design.copy(domainType = DomainType.ED, meta = stripTimingAnnotations(design.meta))
            case domain: DomainBlock =>
              domain.copy(domainType = DomainType.ED, meta = stripTimingAnnotations(domain.meta))
          domainOwner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)
      }
      // Transfer the resolved `@timing.clock` constraint from the owner domain onto the
      // clock input port. In RT the domain owner is the canonical carrier of the timing
      // configuration, but once we lower to ED the owner's timing annotations are stripped
      // (see the owner case above), so the clock rate/edge would otherwise be lost. Moving
      // the annotation onto the port keeps it available for downstream timing-constraint
      // generation (e.g. the SDC `create_clock` in `BuilderProjectTimingConstraints`, which
      // reads `@timing.clock` off the top-level clock port).
      val clkPatchList = firstPart.members.flatMap {
        case dcl: DFVal.Dcl if dcl.isClkDcl && dcl.isPortIn =>
          val alreadyHasClk = dcl.meta.annotations.exists {
            case _: constraints.Timing.Clock => true; case _ => false
          }
          if (alreadyHasClk) None
          else
            resolveTimingOwner(dcl.getOwnerDomain).meta.annotations.collectFirst {
              case c: constraints.Timing.Clock => c
            }.map { clkAnnot =>
              val updatedDcl =
                dcl.setMeta(_.copy(annotations = dcl.meta.annotations :+ clkAnnot))
              dcl -> Patch.Replace(updatedDcl, Patch.Replace.Config.FullReplacement)
            }
        case _ => None
      }
      firstPart.patch(patchList ++ clkPatchList)
    }
  end transformSubDB
end ToED

extension [T: HasDB](t: T)
  def toED(using CompilerOptions): DB = StageRunner.run(ToED)(t.db)
