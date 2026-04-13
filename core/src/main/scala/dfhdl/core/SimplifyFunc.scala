package dfhdl.core
import dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp
import dfhdl.internals.Position

private object SimplifyFunc:
  def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
    // Meta-programming can't safely touch the MutableDB, so all simplifications
    // are skipped in that mode.
    if (dfc.inMetaProgramming) None
    else
      opArgs match
        // These two run even in global context (no owner).
        case ConstFoldAddSubChain(v) => Some(v)
        case MergeAssocFunc(v)       => Some(v)
        // TODO: maybe drop this limitation, if we can make DropStructsVecs work in
        // global context.
        case _ if dfc.ownerOption.isEmpty => None
        case NegateDecimalConst(v)   => Some(v)
        case IdentityOps(v)          => Some(v)
        case SelfCancelling(v)       => Some(v)
        case MaxMinWithOffset(v)     => Some(v)
        case AdditiveCancellation(v) => Some(v)
        case _                       => None

  // Checks if an intermediate Func can be merged into the current one.
  // + and * are only merged when the intermediate has the same dfType (non-carry).
  // - is excluded since it's not truly associative for reordering.
  // ++ is only merged for flat DFBits concatenation, not struct/vector/string.
  private def canMergeFunc(
      resultType: ir.DFType,
      op: FuncOp,
      prevFunc: ir.DFVal.Func
  )(using ir.MemberGetSet): Boolean =
    op match
      case FuncOp.++ =>
        resultType.isInstanceOf[ir.DFBits] && prevFunc.dfType.isInstanceOf[ir.DFBits]
      case FuncOp.+ | FuncOp.`*` =>
        prevFunc.dfType == resultType
      case FuncOp.- => false
      case _        => true // &, |, ^, max, min — no carry concept

  // Creates a fresh DFInt32 Const with the current DFC meta, so the outer val
  // binding (if any) can name it.
  private def mkInt32Const(value: BigInt)(using dfc: DFC): ir.DFVal =
    import dfc.getSet
    ir.DFVal.Const(
      ir.DFInt32, Some(value),
      dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags
    ).addMember

  // Re-stamps an anonymous returning DFVal with the current meta so the outer
  // val binding picks up its name, matching the existing pattern in
  // ConstFoldAddSubChain. Named values are returned as-is.
  private def rebindMeta(v: ir.DFVal)(using dfc: DFC): ir.DFVal =
    import dfc.getSet
    if (v.isAnonymous) v.setMeta(_ => dfc.getMeta) else v

  // Extractor for an anonymous DFInt32 Const with a known Int payload.
  private object ConstInt:
    def unapply(v: ir.DFVal)(using ir.MemberGetSet): Option[Int] = v match
      case c: ir.DFVal.Const if c.isAnonymous && c.dfType == ir.DFInt32 =>
        c.data match
          case Some(n: BigInt) if n.isValidInt => Some(n.toInt)
          case _                               => None
      case _ => None

  // Constant-fold DFInt32 add/sub with constant RHS when the LHS is itself
  // a same-shape anonymous Func. This MUST run before the multi-arg merge
  // below, otherwise `p3 + 1 + 1 + 1` would merge into a 4-arg Func instead
  // of folding into `p3 + 3`. Also folds `Const +/- Const` when a prior
  // simplification (e.g., AdditiveCancellation) collapsed the LHS to a Const.
  private object ConstFoldAddSubChain:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (
              ir.DFInt32,
              currentOp @ (FuncOp.+ | FuncOp.-),
              List(
                prevFunc: ir.DFVal.Func,
                currentRHSArg @ ir.DFVal.Const(data = Some(currentRHSData: BigInt))
              )
            )
            if currentRHSArg.isAnonymous && prevFunc.isAnonymous &&
              !dfc.ownerOption.isEmpty =>
          (prevFunc.dfType, prevFunc.op, prevFunc.args.map(_.get)) match
            case (
                  ir.DFInt32,
                  prevOp @ (FuncOp.+ | FuncOp.-),
                  List(prevLHSArg, prevRHSArg @ ir.DFVal.Const(data = Some(prevRHSData: BigInt)))
                ) if prevRHSArg.isAnonymous =>
              val newRHSData = (prevOp, currentOp).runtimeChecked match
                // (x + c1) + c2 => x + (c1 + c2)
                case (FuncOp.+, FuncOp.+) => prevRHSData + currentRHSData
                // (x - c1) - c2 => x - (c1 + c2)
                case (FuncOp.-, FuncOp.-) => prevRHSData + currentRHSData
                // (x + c1) - c2 => x + (c1 - c2)
                case (FuncOp.+, FuncOp.-) => prevRHSData - currentRHSData
                // (x - c1) + c2 => x - (c1 - c2)
                case (FuncOp.-, FuncOp.+) => prevRHSData - currentRHSData
              if (newRHSData == BigInt(0)) Some(rebindMeta(prevLHSArg))
              else
                // Clone prevFunc to avoid destructively modifying shared IR nodes
                val clonedFunc = prevFunc.cloneAnonValueAndDepsHere
                  .asInstanceOf[ir.DFVal.Func]
                val clonedRHSArg = clonedFunc.args.last.get
                  .asInstanceOf[ir.DFVal.Const]
                dfc.mutableDB.setMember(clonedRHSArg, _.copy(data = Some(newRHSData)))
                Some(dfc.mutableDB.setMember(clonedFunc, _.copy(meta = dfc.getMeta)))
            case _ => None
          end match
        // Const +/- Const fold. Runs when the LHS has been collapsed to a
        // bare anonymous Const by a prior extractor (e.g., AdditiveCancellation).
        case (
              ir.DFInt32,
              currentOp @ (FuncOp.+ | FuncOp.-),
              List(
                lhs @ ir.DFVal.Const(data = Some(lhsData: BigInt)),
                rhs @ ir.DFVal.Const(data = Some(rhsData: BigInt))
              )
            )
            if lhs.isAnonymous && rhs.isAnonymous &&
              !dfc.ownerOption.isEmpty =>
          val result = currentOp.runtimeChecked match
            case FuncOp.+ => lhsData + rhsData
            case FuncOp.- => lhsData - rhsData
          Some(
            dfc.mutableDB.setMember(
              lhs,
              _.copy(data = Some(result), meta = dfc.getMeta)
            )
          )
        case _ => None
      end match
    end unapply
  end ConstFoldAddSubChain

  // Merge consecutive same-op anonymous Funcs for associative operations.
  // E.g., `a + b + c` becomes Func(+, [a, b, c]) instead of nested binary Funcs.
  // For left-associative chains, only the first arg can be an absorbed Func.
  // Multi-referenced nodes (same Func used twice) are not merged.
  // Runs even in global context (no owner), unlike the later simplifications.
  private object MergeAssocFunc:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (dfType, op, (prevFunc: ir.DFVal.Func) :: rest)
            if ir.DFVal.Func.Op.associativeSet.contains(op)
              && prevFunc.op == op
              && prevFunc.isAnonymous
              && !rest.contains(prevFunc)
              && canMergeFunc(dfType, op, prevFunc) =>
          val currentMeta = dfc.getMeta
          val lhsPos = prevFunc.meta.position
          val currentPos = currentMeta.position
          val mergedPos = Position(
            lhsPos.file, lhsPos.lineStart, lhsPos.columnStart,
            currentPos.lineEnd, currentPos.columnEnd
          )
          val meta = currentMeta.copy(position = mergedPos)
          // Reuse absorbed Func's existing arg refs (so they aren't orphaned)
          // and create new refs only for the tail args being appended.
          val newArgRefs: List[ir.DFVal.Ref] =
            prevFunc.args ++ rest.map(_.refTW[ir.DFVal](knownReachable = true))
          val func: ir.DFVal = ir.DFVal.Func(
            dfType, op, newArgRefs,
            dfc.ownerOrEmptyRef, meta, dfc.tags
          )
          // Add positions newFunc at the tail (after any later-created arg deps).
          // Reusing prevFunc.args causes setOriginRefs to update their origin to
          // newFunc, so the absorbed Func can simply be marked ignored.
          func.addMember
          getSet.remove(prevFunc)
          Some(func)
        case _ => None
    end unapply
  end MergeAssocFunc

  // Special case to handle unary negation of anonymous decimal constants.
  private object NegateDecimalConst:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (
              _: ir.DFDecimal,
              FuncOp.unary_-,
              List(const @ ir.DFVal.Const(dfType = _: ir.DFDecimal, data = Some(data: BigInt)))
            ) if (const.isAnonymous || const.asValAny.inDFCPosition) =>
          Some(
            dfc.mutableDB.setMember(
              const,
              _.copy(
                data = Some(-data),
                meta = dfc.getMeta
              )
            )
          )
        case _ => None
    end unapply
  end NegateDecimalConst

  // DFInt32 identity simplifications against literal 0/1.
  private object IdentityOps:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        // x + 0 / 0 + x / x - 0  ->  x
        case (ir.DFInt32, FuncOp.+, List(x, ConstInt(0))) => Some(rebindMeta(x))
        case (ir.DFInt32, FuncOp.+, List(ConstInt(0), x)) => Some(rebindMeta(x))
        case (ir.DFInt32, FuncOp.-, List(x, ConstInt(0))) => Some(rebindMeta(x))
        // x * 1 / 1 * x  ->  x
        case (ir.DFInt32, FuncOp.`*`, List(x, ConstInt(1))) => Some(rebindMeta(x))
        case (ir.DFInt32, FuncOp.`*`, List(ConstInt(1), x)) => Some(rebindMeta(x))
        // x * 0 / 0 * x  ->  0
        case (ir.DFInt32, FuncOp.`*`, List(_, c @ ir.DFVal.Const(data = Some(d: BigInt))))
            if d == BigInt(0) && c.isAnonymous =>
          Some(
            dfc.mutableDB.setMember(c, _.copy(meta = dfc.getMeta))
          )
        case (ir.DFInt32, FuncOp.`*`, List(c @ ir.DFVal.Const(data = Some(d: BigInt)), _))
            if d == BigInt(0) && c.isAnonymous =>
          Some(
            dfc.mutableDB.setMember(c, _.copy(meta = dfc.getMeta))
          )
        case _ => None
    end unapply
  end IdentityOps

  // Self-cancellation: two structurally equal operands.
  private object SelfCancelling:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        // a - a  ->  0
        case (ir.DFInt32, FuncOp.-, List(a, b)) if a =~ b =>
          Some(mkInt32Const(0))
        // max(a, a) / min(a, a)  ->  a
        case (ir.DFInt32, FuncOp.max | FuncOp.min, List(a, b)) if a =~ b =>
          Some(rebindMeta(a))
        case _ => None
    end unapply
  end SelfCancelling

  // max/min between a value and itself plus a constant offset:
  //   max(a, a + c) => (a + c) if c > 0, else a
  //   min(a, a + c) => a if c > 0, else (a + c)
  // Commutative in both operand orderings.
  private object MaxMinWithOffset:
    // If `candidate` is an anonymous DFInt32 additive Func `base (+|-) Const c`,
    // returns the signed offset relative to `base`.
    private def offsetFromBase(candidate: ir.DFVal, base: ir.DFVal)(using
        ir.MemberGetSet
    ): Option[Int] =
      candidate match
        case f: ir.DFVal.Func if f.isAnonymous && f.dfType == ir.DFInt32 =>
          (f.op, f.args.map(_.get)) match
            case (FuncOp.+, List(l, ConstInt(c))) if l =~ base => Some(c)
            case (FuncOp.-, List(l, ConstInt(c))) if l =~ base => Some(-c)
            case _                                             => None
        case _ => None

    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (ir.DFInt32, op @ (FuncOp.max | FuncOp.min), List(a, b)) =>
          val chooseMax = op == FuncOp.max
          def pick(bigger: ir.DFVal, smaller: ir.DFVal): ir.DFVal =
            if (chooseMax) rebindMeta(bigger) else rebindMeta(smaller)
          // b = a + c
          offsetFromBase(b, a) match
            case Some(c) if c > 0 => Some(pick(b, a))
            case Some(c) if c < 0 => Some(pick(a, b))
            case _ =>
              // a = b + c
              offsetFromBase(a, b) match
                case Some(c) if c > 0 => Some(pick(a, b))
                case Some(c) if c < 0 => Some(pick(b, a))
                case _                => None
        case _ => None
      end match
    end unapply
  end MaxMinWithOffset

  // Cancels opposing +/- terms of the same non-constant DFVal across a
  // left-associative DFInt32 additive chain. Handles e.g. `(x - 1) - x => -1`,
  // which together with Const+Const folding handles `x - 1 - x + 5 => 4`.
  private object AdditiveCancellation:
    // Walk a left-associative +/- chain rooted at `v` and return its terms
    // as (sign, DFVal). Non-chain leaves become a single positive term.
    private def collectChain(v: ir.DFVal)(using ir.MemberGetSet): List[(Int, ir.DFVal)] =
      def loop(v: ir.DFVal, sign: Int, acc: List[(Int, ir.DFVal)]): List[(Int, ir.DFVal)] =
        v match
          case f: ir.DFVal.Func
              if f.isAnonymous && f.dfType == ir.DFInt32 &&
                (f.op == FuncOp.+ || f.op == FuncOp.-) && f.args.size == 2 =>
            val List(lhs, rhs) = f.args.map(_.get): @unchecked
            val rhsSign = if (f.op == FuncOp.+) sign else -sign
            loop(lhs, sign, (rhsSign, rhs) :: acc)
          case _ => (sign, v) :: acc
      loop(v, 1, Nil)

    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (ir.DFInt32, currentOp @ (FuncOp.+ | FuncOp.-), List(prev, curr))
            if prev.isAnonymous =>
          val chain = collectChain(prev) :+ ((if (currentOp == FuncOp.+) 1 else -1, curr))
          if (chain.size < 2) None
          else
            // Find two terms with opposite signs whose DFVals are =~.
            val indexed = chain.zipWithIndex
            val pairOpt: Option[(Int, Int)] = indexed.iterator.collectFirst {
              case ((s1, t1), i) =>
                indexed.iterator.collectFirst {
                  case ((s2, t2), j) if j != i && s1 == -s2 && t1 =~ t2 =>
                    (i, j)
                }
            }.flatten
            pairOpt.flatMap { case (i, j) =>
              val remaining = indexed.collect { case (term, k) if k != i && k != j => term }
              rebuildChain(remaining)
            }
        case _ => None
      end match
    end unapply

    // Rebuild the chain from the remaining terms after a pair was cancelled.
    // Only handles the residues that show up for the currently-targeted
    // simplifications: all-const (fold to a single Const) or a single positive
    // non-const term. More general residues return None so the chain is left
    // intact for a later (possibly future) pass.
    private def rebuildChain(
        terms: List[(Int, ir.DFVal)]
    )(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      if (terms.isEmpty) Some(mkInt32Const(0))
      else
        val (constSum, nonConsts) =
          terms.foldLeft((BigInt(0), List.empty[(Int, ir.DFVal)])) {
            case ((sum, rest), (s, ir.DFVal.Const(data = Some(d: BigInt)))) =>
              (sum + BigInt(s) * d, rest)
            case ((sum, rest), term) => (sum, term :: rest)
          }
        val nonConstOrdered = nonConsts.reverse
        (nonConstOrdered, constSum) match
          case (Nil, c)                            => Some(mkInt32Const(c))
          case (List((1, t)), c) if c == BigInt(0) => Some(rebindMeta(t))
          case _                                   => None
    end rebuildChain
  end AdditiveCancellation
end SimplifyFunc
