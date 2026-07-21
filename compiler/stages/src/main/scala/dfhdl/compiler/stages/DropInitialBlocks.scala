package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable
import scala.collection.immutable.ListMap

//format: off
/** Lowers the `initial` blocks remaining after [[SplitInitialBlocks]] into forms VHDL can
  * express (VHDL has no `initial` construct). Runs only under the VHDL backend. RT-domain
  * blocks whose resolved timing configuration has a reset are skipped: `ToED` plants their
  * content into the register reset branch.
  *
  * ==Rule 1: Init-function conversion==
  *
  * A block assigning a single declaration, whose content is computable from constants
  * alone, is lowered into a generated STATIC FUNCTION (a `Def` design in the Static
  * domain): the body computes the initial value into a local variable and returns it, and
  * the declaration's `init` becomes a `Func`/`Op.Def` call of that function. VHDL prints
  * this as the classic init-function idiom:
  * {{{
  * pure function vec_init return t_vec is
  *   variable vec : t_vec;
  * begin
  *   for i in 0 to 3 loop
  *     vec(i) := to_signed(0, 16);
  *   end loop;
  *   return vec;
  * end function;
  * signal vec : t_vec := vec_init;
  * }}}
  * Captured design-local constants become phantom input formals (exactly like frontend
  * method captures), keeping the def design self-contained; the call carries them as its
  * actuals. Reads of the initialized declaration itself are redirected to the function's
  * local variable, so sequential self-reading content (`v := 0; v := v + 1`) converts
  * faithfully (VHDL variable semantics preserve the in-block order).
  *
  * The generated def design is extracted into its own sub-DB (nested design blocks live
  * exclusively in their own sub-DB under the hierarchical model, reachable from the parent
  * through the call's `Op.Def` key), which is why this stage manages the sub-DB map itself
  * rather than extending `HierarchyStage`.
  *
  * ==Rule 2: One-shot process conversion==
  *
  * Any ED-domain block Rule 1 cannot convert (simulation-only content, non-constant
  * reads, multi-declaration cross-reading blocks) becomes a `process` (empty sensitivity)
  * terminated by an endless `wait`, which VHDL prints as the classic one-shot
  * `process ... wait; end process;` form, preserving the block's sequential execution at
  * time zero.
  * {{{
  * // Before
  * initial:
  *   println("simulation started")
  *
  * // After
  * process:
  *   println("simulation started")
  *   wait
  * }}}
  *
  * An RT-domain block without a reset that Rule 1 cannot convert (a type or constant
  * parameterized by a design-local value) is left untouched, a documented residual gap
  * (the VHDL printer rejects it).
  */
//format: on
case object DropInitialBlocks extends Stage:
  def dependencies: List[Stage] = List(SplitInitialBlocks)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVHDL

  // A type is portable into the generated def design only when its references (if any)
  // target globals; a type parameterized by a design-local value would break the def
  // design's self-containment.
  private def globalTypeRefsOnly(dfType: DFType)(using MemberGetSet): Boolean =
    dfType.getRefs.forall(_.get.isGlobal)

  // Design-local constant values the block captures, in first-encounter order. Each becomes
  // a phantom input formal of the generated static function (mirroring frontend method
  // captures). Returns None when the block cannot become a static function body: a
  // reference to a non-constant design-local value, or a type parameterized by a
  // design-local value.
  private def initFuncCaptures(
      dcl: DFVal.Dcl,
      blockMembers: List[DFMember]
  )(using MemberGetSet): Option[List[DFVal]] =
    val inBlock = blockMembers.toSet
    val captures = mutable.LinkedHashSet.empty[DFVal]
    var convertible = globalTypeRefsOnly(dcl.dfType)
    blockMembers.foreach { m =>
      m.getRefs.foreach { ref =>
        if (convertible)
          val isTypeRef = ref.isInstanceOf[DFRef.TypeRef]
          ref.get match
            case _: DFMember.Empty        =>
            case t if t == dcl            => if (isTypeRef) convertible = false
            case t if inBlock.contains(t) =>
            case t: DFVal if t.isGlobal   =>
            case t: DFVal if !isTypeRef && t.isConst && globalTypeRefsOnly(t.dfType) =>
              captures += t
            case _ => convertible = false
      }
    }
    if (convertible) Some(captures.toList) else None
  end initFuncCaptures

  // The outcome of one Rule 1 conversion: the dcl-anchored patch, plus the def design and
  // its content members (by identity, as they will appear in the patched sub-DB) for the
  // sub-DB extraction below.
  private final class InitFuncResult(
      val patch: (DFMember, Patch),
      val defBlock: DFDesignBlock,
      val defContent: List[DFMember]
  )

  // Rule 1: generate the static function def design and rewire the declaration's init to
  // call it. The def design mirrors the frontend's method elaboration shape: phantom
  // capture formals first, then the body (the block cloned with the initialized
  // declaration redirected to a local variable), then an ident of the local variable
  // connected to the return output port. The call (`Func` with `Op.Def` keyed by the def
  // block's `ownerRef`) is anonymous, so it prints inline as the declaration's `init`.
  private def initFunctionConversion(
      pb: ProcessBlock,
      dcl: DFVal.Dcl,
      blockMembers: List[DFMember],
      captures: List[DFVal]
  )(using MemberGetSet, RefGen): InitFuncResult =
    val defContent = List.newBuilder[DFMember]
    var defBlockResult: Option[DFDesignBlock] = None
    val dsn = new MetaDesign(
      dcl,
      Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
    ):
      import dfhdl.core.refTW
      private def refTo[M <: ir.DFMember, O <: ir.DFMember](member: M): ir.DFRef.TwoWay[M, O] =
        dfc.mutableDB.newRefFor(dfc.refGen.genTwoWay[M, O], member)
      // fresh copies of a design-local type keep their (global-only) reference targets
      private def cloneTypeOf(origType: ir.DFType): ir.DFType =
        val cloned = origType.copyWithNewRefs(using dfc.refGen)
        origType.getRefs.lazyZip(cloned.getRefs).foreach { (ref, clonedRef) =>
          dfc.mutableDB.newRefFor(clonedRef, ref.get)
        }
        cloned
      // the def design block, named after the initialized declaration. Its `ownerRef` is
      // the hierarchy key of its sub-DB; the extraction below leaves it unbound.
      val defBlockIR = ir.DFDesignBlock(
        ir.DomainType.Static,
        ir.DFDesignBlock.InstMode.Def,
        dfc.ownerOrEmptyRef,
        dcl.meta.setAnnotations(Nil).setName(s"${dcl.getName}_init"),
        ir.DFTags.empty
      )
      dfc.mutableDB.addMember(defBlockIR)
      defBlockResult = Some(defBlockIR)
      private def plantUnder[M <: ir.DFMember](m: M): M =
        dfc.mutableDB.addMember(m)
        dfc.mutableDB.newRefFor(m.ownerRef, defBlockIR)
        defContent += m
        m
      // phantom input formals for the captured design-local constants (formal member order
      // defines the call's actuals order)
      val captureFormals: List[(ir.DFVal, ir.DFVal.Dcl)] = captures.zipWithIndex.map {
        (cap, idx) =>
          val capMeta =
            if (cap.meta.isAnonymous)
              dcl.meta.setAnnotations(Nil).setName(s"${dcl.getName}_init_arg$idx")
            else cap.meta.setAnnotations(Nil)
          cap -> plantUnder(
            ir.DFVal.Dcl(
              cloneTypeOf(cap.dfType),
              ir.DFVal.Modifier(ir.DFVal.Modifier.Dir.IN, ir.DFVal.Modifier.Special.Ordinary),
              Nil,
              dfc.ownerOrEmptyRef,
              capMeta,
              ir.DFTags.empty.tag(ir.PhantomTag)
            )
          )
      }
      val captureMap: Map[ir.DFMember, ir.DFVal.Dcl] = captureFormals.toMap
      // the local variable the body assigns and returns, named like the declaration
      val localVarIR = plantUnder(
        ir.DFVal.Dcl(
          cloneTypeOf(dcl.dfType),
          ir.DFVal.Modifier(ir.DFVal.Modifier.Dir.VAR, ir.DFVal.Modifier.Special.Ordinary),
          Nil,
          dfc.ownerOrEmptyRef,
          dcl.meta.setAnnotations(Nil),
          ir.DFTags.empty
        )
      )
      // the body: the block cloned under the def design, with references to the
      // initialized declaration redirected to the local variable and captured constants
      // redirected to their phantom formals
      val cloneMap: Map[ir.DFMember, ir.DFMember] =
        blockMembers.map(m => m -> m.copyWithNewRefs(using dfc.refGen)).toMap
      blockMembers.foreach { m =>
        val cloned = cloneMap(m)
        dfc.mutableDB.addMember(cloned)
        defContent += cloned
        val owner = cloneMap.get(m.getOwner) match
          case Some(clonedOwner: ir.DFOwner) => clonedOwner
          case _                             => defBlockIR
        dfc.mutableDB.newRefFor(cloned.ownerRef, owner)
        m.getRefs.lazyZip(cloned.getRefs).foreach { (ref, clonedRef) =>
          val origTarget = ref.get
          val target =
            if (origTarget == dcl) localVarIR
            else
              captureMap.get(origTarget) match
                case Some(formal) => formal
                case None         => cloneMap.getOrElse(origTarget, origTarget)
          dfc.mutableDB.newRefFor(clonedRef, target)
        }
      }
      // the return path: an ident of the local variable connected to the output port
      val retIdentIR = plantUnder(
        ir.DFVal.Alias.AsIs(
          cloneTypeOf(dcl.dfType),
          refTo[ir.DFVal, ir.DFVal.Alias.Partial](localVarIR),
          dfc.ownerOrEmptyRef,
          dcl.meta.anonymize.setAnnotations(Nil),
          ir.DFTags.empty.tag(ir.IdentTag)
        )
      )
      val outPortIR = plantUnder(
        ir.DFVal.Dcl(
          cloneTypeOf(dcl.dfType),
          ir.DFVal.Modifier(ir.DFVal.Modifier.Dir.OUT, ir.DFVal.Modifier.Special.Ordinary),
          Nil,
          dfc.ownerOrEmptyRef,
          dcl.meta.setAnnotations(Nil).setName("o"),
          ir.DFTags.empty
        )
      )
      plantUnder(
        ir.DFNet(
          refTo[ir.DFVal, ir.DFNet](outPortIR),
          ir.DFNet.Op.Connection,
          refTo[ir.DFVal, ir.DFNet](retIdentIR),
          dfc.ownerOrEmptyRef,
          dcl.meta.anonymize.setAnnotations(Nil),
          ir.DFTags.empty
        )
      )
      // the call, in the parent design, carrying the captured constants as actuals; it is
      // anonymous so it prints inline as the declaration's `init` expression
      val callIR = ir.DFVal.Func(
        cloneTypeOf(dcl.dfType),
        ir.DFVal.Func.Op.Def(ir.StaticRef(defBlockIR.ownerRef)),
        captures.map(cap => refTo[ir.DFVal, ir.DFMember](cap)),
        dfc.ownerOrEmptyRef,
        dcl.meta.anonymize.setAnnotations(Nil),
        ir.DFTags.empty
      )
      dfc.mutableDB.addMember(callIR)
      // the declaration, now initialized by the call (replaces the original via
      // ReplaceWithLast)
      plantMember(dcl.copy(initRefList = List(callIR.refTW[ir.DFVal.Dcl](using dfc))))
    InitFuncResult(dsn.patch, defBlockResult.get, defContent.result())
  end initFunctionConversion

  // Extract the stage-created def designs out of the patched parent sub-DB into their own
  // sub-DBs, mirroring `oldToNew`'s partitioning: a def sub-DB's members are its reachable
  // globals closure, the design block, then its locals; each refTable carries exactly the
  // refs its own members emit.
  private def extractDefSubDBs(
      patched: DB,
      results: List[InitFuncResult]
  ): (DB, List[(StaticRef, DB)]) =
    given MemberGetSet = patched.getSet
    val allDefMembers: Set[DFMember] =
      results.view.flatMap(r => r.defBlock :: r.defContent).toSet
    val defOwnerRefs: Set[DFRefAny] = results.view.map(r => r.defBlock.ownerRef: DFRefAny).toSet
    val allGlobalsOrdered: List[DFMember] = patched.members.collect {
      case g: DFVal.CanBeGlobal if g.isGlobal => g
    }
    def globalsClosure(dbMembers: Iterable[DFMember]): List[DFMember] =
      val reachable = mutable.Set.empty[DFMember]
      def pull(target: DFMember): Unit = target match
        case g: DFVal.CanBeGlobal if g.isGlobal && !reachable.contains(g) =>
          reachable += g
          g.getRefs.foreach(r => patched.refTable.get(r).foreach(pull))
        case _ =>
      dbMembers.foreach { m =>
        m.getRefs.foreach(r => patched.refTable.get(r).foreach(pull))
      }
      allGlobalsOrdered.filter(reachable.contains)
    def refsFor(dbMembers: Iterable[DFMember]): Map[DFRefAny, DFMember] =
      val result = mutable.Map.empty[DFRefAny, DFMember]
      dbMembers.foreach { m =>
        if (!defOwnerRefs.contains(m.ownerRef))
          patched.refTable.get(m.ownerRef).foreach(t => result(m.ownerRef) = t)
        m.getRefs.foreach(r => patched.refTable.get(r).foreach(t => result(r) = t))
      }
      result.toMap
    val parentMembers = patched.members.filterNot(allDefMembers)
    val parentDB = patched.update(members = parentMembers, refTable = refsFor(parentMembers))
    val defDBs = results.map { r =>
      val locals = patched.members.filter(r.defContent.toSet)
      val dbMembers = globalsClosure(r.defBlock :: locals) ::: r.defBlock :: locals
      // the def block heads its own sub-DB: its `ownerRef` resolves to `DFMember.Empty`
      // (`isTop` within the sub-DB), while doubling as the sub-DB's hierarchy key
      val defRefTable = refsFor(dbMembers) + (r.defBlock.ownerRef -> DFMember.Empty)
      StaticRef(r.defBlock.ownerRef) ->
        patched.update(members = dbMembers, refTable = defRefTable)
    }
    (parentDB, defDBs)
  end extractDefSubDBs

  // Rule 1 conversions and Rule 2 one-shot patches are emitted as one bundled patch list
  // per sub-DB; Rule 1 def designs are then extracted into their own sub-DBs.
  private def transformSubDB(
      subDB: DB
  )(using MemberGetSet, CompilerOptions, RefGen): (DB, List[(StaticRef, DB)]) =
    def endlessWait()(using dfhdl.core.DFC): Unit =
      // an endless wait: `ir.Wait` with an anonymous const-`false` trigger
      // (printed by the VHDL backend as a bare `wait;`)
      dfhdl.core.Wait(dfhdl.core.DFVal.Const(dfhdl.core.DFBool, Some(false)))

    // Rule 2: convert the block itself, in place, into a one-shot process (empty sensitivity
    // list + endless wait). The Replace patch must precede the Add in the returned list so
    // the wait's owner reference is redirected to the updated block.
    def oneShotProcessPatches(pb: ProcessBlock): List[(DFMember, Patch)] =
      val updated = pb.copy(sensitivity = ProcessBlock.Sensitivity.List(Nil))
      val waitDsn = new MetaDesign(pb, Patch.Add.Config.InsideLast):
        endlessWait()(using dfc)
      List(pb -> Patch.Replace(updated, Patch.Replace.Config.FullReplacement), waitDsn.patch)

    val defResults = mutable.ListBuffer.empty[InitFuncResult]
    val patchList: List[(DFMember, Patch)] = subDB.members.flatMap {
      // RT blocks with a resolved reset are ToED's (planted into the reset branch)
      case pb: ProcessBlock if pb.isInitial && !(pb.isInRTDomain && pb.hasResolvedRstCfg) =>
        val blockMembers = pb.members(MemberView.Flattened)
        val funcPatches = assignedDcls(blockMembers) match
          case dcl :: Nil =>
            initFuncCaptures(dcl, blockMembers).map { captures =>
              val result = initFunctionConversion(pb, dcl, blockMembers, captures)
              defResults += result
              result.patch :: (pb :: blockMembers).map(_ -> Patch.Remove())
            }
          case _ => None
        funcPatches.getOrElse {
          if (pb.isInEDDomain) oneShotProcessPatches(pb)
          else Nil // RT without a reset, non-convertible: a documented residual gap
        }
      case _ => Nil
    }
    val patched = subDB.patch(patchList)
    if (defResults.isEmpty) (patched, Nil)
    else extractDefSubDBs(patched, defResults.toList)
  end transformSubDB

  // Manages the sub-DB map itself (mirroring `HierarchyStage.transform`) because Rule 1
  // conversions ADD def-design sub-DBs, which `HierarchyStage` cannot express.
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given refGen: RefGen = RefGen.fromGetSet
    var changed = false
    val newDefSubDBs = mutable.ListBuffer.empty[(StaticRef, DB)]
    val transformedSubs: ListMap[StaticRef, DB] =
      designDB.subDBs.map { case (k, sub) =>
        val (result, defDBs) = transformSubDB(sub)(using sub.getSet, co, refGen)
        if (!(result eq sub)) changed = true
        newDefSubDBs ++= defDBs
        k -> result
      }
    if (!changed) designDB
    else designDB.update(subDBs = transformedSubs ++ newDefSubDBs)
  end transform
end DropInitialBlocks

extension [T: HasDB](t: T)
  def dropInitialBlocks(using CompilerOptions): DB =
    StageRunner.run(DropInitialBlocks)(t.db)
