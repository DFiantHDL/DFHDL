package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
import scala.annotation.tailrec
//format: off
/** This stage simplifies RT-domain operations into lower-level constructs that downstream stages
  * can process. It handles four kinds of transformations: rising/falling edge predicates, boolean
  * wait statements, cycle-count wait statements, and for loops.
  *
  * ==Rule 1: Rising/falling edge simplification==
  *
  * `rising` and `falling` predicates on a `Bit` signal are replaced with register-based edge
  * detection expressions:
  * {{{
  * // Before
  * i.rising
  * i.falling
  *
  * // After
  * (!i.reg(1, init = 1)) && i
  * i.reg(1, init = 0) && (!i)
  * }}}
  *
  * ==Rule 2: Boolean wait → while loop==
  *
  * `waitWhile(cond)` / `waitUntil(cond)` statements (including those with rising/falling edge
  * triggers) are replaced with `while` loops. If the wait has a name, the generated while loop
  * takes that name:
  * {{{
  * // Before
  * waitWhile(i)
  * val MyWait = waitUntil(i)
  * waitUntil(i.falling)
  *
  * // After
  * while (i)
  * end while
  * val MyWait = while (!i)
  * end MyWait
  * while ((!i.reg(1, init = 0)) || i)
  * end while
  * }}}
  *
  * ==Rule 3: Cycle-count wait → while loop with counter==
  *
  * A wait with a cycle count (`N.cy.wait`) is replaced by a `VAR.REG` counter assigned to zero
  * before the loop and a `while` loop that counts up to `N-1`. A single-cycle wait (`1.cy.wait`)
  * is left unchanged (no loop needed). If the wait has a name (`val N = X.cy.wait`), the counter
  * register is prefixed with `N_`:
  * {{{
  * // Before
  * x.din := 1
  * 50000000.cy.wait
  * val MyWait = waitParam.cy.wait
  *
  * // After
  * x.din := 1
  * val waitCnt = UInt(26) <> VAR.REG
  * waitCnt.din := d"26'0"
  * while (waitCnt != d"26'49999999")
  *   waitCnt.din := waitCnt + d"26'1"
  * end while
  * val MyWait_waitCnt = UInt(26) <> VAR.REG
  * MyWait_waitCnt.din := d"26'0"
  * val MyWait = while (MyWait_waitCnt != (waitParam - d"26'1"))
  *   MyWait_waitCnt.din := MyWait_waitCnt + d"26'1"
  * end MyWait
  * }}}
  *
  * ==Rule 4: For loop → while loop with iterator==
  *
  * `for` loops that are inside an RT process are replaced by a `Int <> VAR.REG` iterator assigned
  * to the range start before the loop, and a `while` loop whose body ends with the iterator
  * increment. The comparison operator is `<` for `until` (exclusive) and `<=` for `to` (inclusive);
  * for negative steps the operators are `>` and `>=` respectively. For loops tagged as `COMB_LOOP`,
  * outside the RT domain, or outside a process block are left unchanged.
  *
  * Iterator naming:
  *   - anonymous `for (i <- ...)`: iterator register keeps the original name (`i`)
  *   - named `val Name = for (i <- ...)`: iterator register is named `Name_i`
  * {{{
  * // Before
  * x.din := 1
  * for (i <- 0 until 4)
  *   x.din := 0
  * x.din := 1
  *
  * // After
  * x.din := 1
  * val i = Int <> VAR.REG
  * i.din := 0
  * while (i < 4)
  *   x.din := 0
  *   i.din := i + 1
  * end while
  * x.din := 1
  * }}}
  */
//format: on
case object SimplifyRTOps extends HierarchyStage:
  def dependencies: List[Stage] = List(DropTimedRTWaits)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons, DFHDLUniqueNames, DropLocalDcls)

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    transformRepeatedly(subDB)

  // Nested for loops must not be rewritten in the same pass: the inner loop is a body member of the
  // outer loop, so transforming both at once (each via `ReplaceWithLast(ChangeRefAndRemove)` plus an
  // `After` increment patch) interleaves their patches and mis-orders the inner while-block's members
  // relative to the outer one, breaking the flat-list ownership invariant. Instead, rewrite one
  // nesting level per pass (innermost transformable for loops first) until none remain. The edge and
  // wait rules only match their original IR shapes, so they fire once on the first pass and produce
  // no further patches on later passes.
  @tailrec private def transformRepeatedly(db: DB)(using CompilerOptions, RefGen): DB =
    given MemberGetSet = db.getSet
    val patches = collectPatches
    if patches.isEmpty then db
    else transformRepeatedly(db.patch(patches))

  // A for loop this stage rewrites into a while loop (Rule 4).
  private def isTransformableForLoop(fb: DFLoop.DFForBlock)(using MemberGetSet): Boolean =
    fb.isInRTDomain && !fb.isCombinational && fb.isInProcess

  private def collectPatches(using MemberGetSet, CompilerOptions, RefGen): List[(DFMember, Patch)] =
    extension (dfVal: DFVal)
      def isAnonReferencedByWait: Boolean = dfVal.isAnonymous && dfVal.originMembers.view.exists {
        case _: Wait => true
        case _       => false
      }
    subDB.members.view.flatMap {
      case trigger @ DFVal.Func(
            _,
            op @ (FuncOp.rising | FuncOp.falling),
            List(DFRef(arg)),
            _,
            _,
            _
          ) if trigger.isInRTDomain && !trigger.isAnonReferencedByWait =>
        val dsn = new MetaDesign(
          trigger,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT
        ):
          val argFE = arg.asValOf[dfhdl.core.DFBit]
          op match
            case FuncOp.rising =>
              (!argFE.reg(1, init = 1)).&&(argFE)(using dfc.setMeta(trigger.meta))
            case FuncOp.falling =>
              argFE.reg(1, init = 0).&&(!argFE)(using dfc.setMeta(trigger.meta))
        Some(dsn.patch)

      case waitMember @ Wait(triggerRef = DFRef(trigger @ DFBoolOrBit.Val(_)))
          if waitMember.isInRTDomain =>
        // Create a while loop with a cycle wait
        val dsn = new MetaDesign(
          waitMember,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
          dfhdl.core.DomainType.RT
        ):
          // If the trigger is a rising or falling edge, we need to negate it
          val fixedTrigger = trigger match
            case DFVal.Func(op = op @ (FuncOp.rising | FuncOp.falling), args = List(DFRef(arg))) =>
              if (trigger.isAnonReferencedByWait)
                val argFE = arg.asValOf[dfhdl.core.DFBit]
                op match
                  case FuncOp.rising =>
                    argFE.reg(1, init = 1).||(!argFE)(using dfc.setMeta(trigger.meta))
                  case FuncOp.falling =>
                    (!argFE.reg(1, init = 0)).||(argFE)(using dfc.setMeta(trigger.meta))
              else !(trigger.asValOf[dfhdl.core.DFBool])
            case _ =>
              trigger.asValOf[dfhdl.core.DFBoolOrBit]
          val whileBlock =
            dfhdl.core.DFWhile.Block(fixedTrigger)(using dfc.setMeta(waitMember.meta))
          dfc.enterOwner(whileBlock)
          dfc.exitOwner()
        Some(dsn.patch)
      // replace wait statements with time durations with cycles
      case waitMember @ Wait(triggerRef = DFRef(cyclesVal @ DFDecimal.Val(DFUInt(_)))) =>
        val replaceWithWhile = cyclesVal match
          // if the number of cycles is 1, then there is no need to create a loop.
          case DFVal.Const(data = Some(value: BigInt)) if value == 1 => false
          case _                                                     => true
        if (replaceWithWhile)
          val dsn = new MetaDesign(
            waitMember,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement),
            dfhdl.core.DomainType.RT
          ):
            val iterType = cyclesVal.asValOf[UInt[Int]].dfType
            // TODO: unclear why we cannot directly use 0 and 1 here, but there is indication that
            // the bug originates from a conversion from UInt[1] to the iterType. It could be that the
            // methods under core.DFVal.Alias are not working as intended because of meta-programming.
            val initZero = dfhdl.core.DFVal.Const(iterType, Some(BigInt(0)))
            val waitCntName = waitMember.meta.nameOpt match
              case Some(waitName) => s"${waitName}_waitCnt"
              case None           => "waitCnt"
            val waitCnt = iterType.<>(VAR.REG)(using dfc.setName(waitCntName))
            waitCnt.din := initZero
            // the upper bound for the while loop count
            val upperBound = cyclesVal match
              // the upper bound is reduced to a simpler form when the number of cycles is a constant anonymous value
              case DFVal.Const(data = Some(value: BigInt)) if cyclesVal.isAnonymous =>
                dfhdl.core.DFVal.Const(iterType, Some(value - 1))
              // the upper bound is an expression when the number of cycles is a variable
              case _ =>
                cyclesVal.asValOf[UInt[Int]] - dfhdl.core.DFVal.Const(iterType, Some(BigInt(1)))
            val whileBlock =
              dfhdl.core.DFWhile.Block(waitCnt != upperBound)(using dfc.setMeta(waitMember.meta))
            dfc.enterOwner(whileBlock)
            waitCnt.din := waitCnt + dfhdl.core.DFVal.Const(iterType, Some(BigInt(1)))
            dfc.exitOwner()
          Some(dsn.patch)
        else None
        end if

      // replace RT for loops with while loops + iterator VAR.REG + increment at end of body.
      // Only rewrite the innermost transformable for loop in each pass (one whose body contains no
      // further transformable for loop); outer loops are handled in subsequent passes (see
      // `transformRepeatedly`).
      case forBlock: DFLoop.DFForBlock
          if isTransformableForLoop(forBlock) &&
            !forBlock.members(MemberView.Flattened).exists {
              case inner: DFLoop.DFForBlock => isTransformableForLoop(inner)
              case _                        => false
            } =>
        val iteratorDcl = forBlock.iteratorRef.get
        val range = forBlock.rangeRef.get
        val startBigInt: BigInt = range.startRef.get match
          case DFVal.Const(data = Some(v: BigInt)) => v
          case _                                   => BigInt(0)
        val stepBigInt: BigInt = range.stepRef.get match
          case DFVal.Const(data = Some(v: BigInt)) => v
          case _                                   => BigInt(1)
        val endValIR = range.endRef.get
        val rangeOp = range.op
        val iterName = forBlock.meta.nameOpt match
          case None       => iteratorDcl.getName
          case Some(name) => s"${name}_${iteratorDcl.getName}"
        val forBodyMembers = forBlock.members(MemberView.Folded)
        // M1: create iterator VAR.REG + guard + whileBlock, replacing forBlock.
        // ReplaceWithLast(ChangeRefAndRemove) makes whileBlock (last M1 member) replace forBlock,
        // redirecting ALL refs to forBlock (including body members' ownerRefs) to whileBlock.
        val m1 = new MetaDesign(
          forBlock,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
          dfhdl.core.DomainType.RT
        ):
          val startConst = dfhdl.core.DFVal.Const(dfhdl.core.DFInt32, Some(startBigInt))
          val newIterDcl = dfhdl.core.DFInt32.<>(VAR.REG)(using dfc.setName(iterName))
          newIterDcl.din := startConst
          val endVal = endValIR.asValOf[dfhdl.core.DFInt32]
          val guard = (rangeOp, stepBigInt.signum) match
            case (DFRange.Op.Until, s) if s >= 0 => newIterDcl < endVal
            case (DFRange.Op.To, s) if s >= 0    => newIterDcl <= endVal
            case (DFRange.Op.Until, _)           => newIterDcl > endVal
            case (DFRange.Op.To, _)              => newIterDcl >= endVal
          val whileBlock = dfhdl.core.DFWhile.Block(guard)(using dfc.setMeta(forBlock.meta))
          dfc.enterOwner(whileBlock)
          dfc.exitOwner()
        val newIterDclIR = m1.newIterDcl.asIR
        val iterDclPatch =
          iteratorDcl -> Patch.Replace(newIterDclIR, Patch.Replace.Config.ChangeRefAndRemove)
        if forBodyMembers.nonEmpty then
          // M2: create the increment (newIterDcl.din := newIterDcl + step) after the last body member.
          // M2's injectedOwner = forBodyMembers.last.getOwner = forBlock (in original DB).
          // After M1's ref redirect (forBlock → whileBlock), M2's members' ownerRefs are also
          // redirected to whileBlock, placing the increment correctly as the last while-body statement.
          val m2 = new MetaDesign(
            forBodyMembers.last,
            Patch.Add.Config.After,
            dfhdl.core.DomainType.RT
          ):
            val stepConst = dfhdl.core.DFVal.Const(dfhdl.core.DFInt32, Some(stepBigInt))
            m1.newIterDcl.din := m1.newIterDcl + stepConst
          List(m1.patch, iterDclPatch, m2.patch)
        else List(m1.patch, iterDclPatch)
        end if

      case _ => None
    }.toList
  end collectPatches
end SimplifyRTOps

extension [T: HasDB](t: T)
  def simplifyRTOps(using CompilerOptions): DB =
    StageRunner.run(SimplifyRTOps)(t.db)
