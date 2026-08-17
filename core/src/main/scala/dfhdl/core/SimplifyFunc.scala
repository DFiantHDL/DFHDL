package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.analysis.stripTypePreservingAliases
import DFVal.Func.Op as FuncOp
import dfhdl.internals.Position

private object SimplifyFunc:
  def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
    // Meta-programming can't safely touch the MutableDB, so all simplifications
    // are skipped in that mode.
    if (dfc.inMetaProgramming) None
    else
      // A global operand (e.g. an object-scoped `Int <> CONST` alias) may be seen here before
      // its first `refTW`, which is what injects the operand's own global context into this
      // run's DB (`injectGlobalCtx`). The extractors below dereference operand refs
      // (`stripTypePreservingAliases`, arg walks), so the injection must happen up front, or
      // the first dereference dies with `Missing ref` (issue #494).
      opArgs._3.foreach(_.injectGlobalCtx())
      opArgs match
        // These three run even in global context (no owner).
        case ConstFoldAddSubChain(v) => Some(v)
        // Must precede MergeAssocFunc, which otherwise consumes the same
        // shape by appending the duplicate operand to the chain.
        case MaxMinChainAbsorb(v) => Some(v)
        case MergeAssocFunc(v)    => Some(v)
        // TODO: maybe drop this limitation, if we can make DropStructsVecs work in
        // global context.
        case _ if dfc.ownerOption.isEmpty => None
        case NegateDecimalConst(v)        => Some(v)
        case IdentityOps(v)               => Some(v)
        case SelfCancelling(v)            => Some(v)
        case MaxMinWithOffset(v)          => Some(v)
        case CompareAgainstMaxMin(v)      => Some(v)
        case AdditiveCancellation(v)      => Some(v)
        case _                            => None
      end match

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
        resultType.isInstanceOf[ir.DFBitsWL] && prevFunc.dfType.isInstanceOf[ir.DFBitsWL]
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

  // Creates a fresh Func with the current DFC meta, for a simplification that rewrites the
  // operation rather than answering with a value that already exists.
  private def mkFunc(dfType: ir.DFType, op: FuncOp, args: List[ir.DFVal])(using
      dfc: DFC
  ): ir.DFVal =
    import dfc.getSet
    ir.DFVal.Func(
      dfType,
      op,
      args.map(_.refTW[ir.DFVal](knownReachable = true)),
      dfc.ownerOrEmptyRef,
      dfc.getMeta,
      dfc.tags
    ).addMember

  // Naming without mutation: a simplification returns an EXISTING value, so a `val` binding's
  // name is applied by wrapping the value in a named Ident rather than by restamping its meta
  // (an anonymous member is never revised; issue #449). With an anonymous context the value is
  // returned untouched and keeps its own meta.
  private def rebindMeta(v: ir.DFVal)(using dfc: DFC): ir.DFVal =
    if (dfc.isAnonymous) v
    else DFVal.Alias.AsIs.ident(v.asValAny).asIR

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
                // Fold by construction: a fresh Const carrying the folded payload plus a fresh
                // Func referencing the original LHS. The superseded chain is left as debris for
                // the end-of-design sweep (an anonymous member is never revised; issue #449).
                val foldedConst = ir.DFVal.Const(
                  ir.DFInt32, Some(newRHSData),
                  dfc.ownerOrEmptyRef, prevRHSArg.meta, dfc.tags
                ).addMember
                Some(
                  ir.DFVal.Func(
                    ir.DFInt32,
                    prevOp,
                    List(
                      prevLHSArg.refTW[ir.DFVal](knownReachable = true),
                      foldedConst.refTW[ir.DFVal](knownReachable = true)
                    ),
                    dfc.ownerOrEmptyRef,
                    dfc.getMeta,
                    dfc.tags
                  ).addMember
                )
              end if
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
          // a fresh folded Const; the operand literals become debris for the sweep
          Some(mkInt32Const(result))
        case _ => None
      end match
    end unapply
  end ConstFoldAddSubChain

  // max/min chain absorption: when one operand is itself a same-op max/min
  // Func that already carries the other operand as one of its arguments, the
  // chain subsumes it (max(max(a, b), b) == max(a, b), likewise for min), so
  // the existing chain value is returned as-is. This keeps unrolled width
  // computations like max(max(max(16, W), W), W) minimized to max(16, W).
  // Runs even in global context (no owner): it only reads the chain and never
  // creates or removes members.
  private object MaxMinChainAbsorb:
    private def chainAbsorbs(chain: ir.DFVal, other: ir.DFVal, op: FuncOp)(using
        dfc: DFC
    ): Boolean =
      import dfc.getSet
      // ident-transparent: the chain and the compared operands may be (named) idents of the
      // actual expressions, e.g. `max(M, b)` with `val M = max(a, b)`
      chain.stripTypePreservingAliases match
        case chainFunc: ir.DFVal.Func if chainFunc.dfType == ir.DFInt32 && chainFunc.op == op =>
          val otherStripped = other.stripTypePreservingAliases
          chainFunc.args.exists(_.get.stripTypePreservingAliases =~ otherStripped)
        case _ => false
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      opArgs match
        case (ir.DFInt32, op @ (FuncOp.max | FuncOp.min), List(a, b)) =>
          if (chainAbsorbs(a, b, op)) Some(rebindMeta(a))
          else if (chainAbsorbs(b, a, op)) Some(rebindMeta(b))
          else None
        case _ => None
    end unapply
  end MaxMinChainAbsorb

  // A comparison between a `max`/`min` and one of its OWN branches decides that branch away.
  // Writing the chain as `max(a, B)` for the branch `a` being compared and `B` for whatever is
  // left of it, every such comparison is either an answer or a comparison of `B` with `a`:
  //
  //   max(a, B) >= a   true          min(a, B) <= a   true
  //   max(a, B) <  a   false         min(a, B) >  a   false
  //   max(a, B) >  a   B >  a        min(a, B) <  a   B <  a
  //   max(a, B) <= a   B <= a        min(a, B) >= a   B >= a
  //   max(a, B) === a  B <= a        min(a, B) === a  B >= a
  //   max(a, B) =!= a  B >  a        min(a, B) =!= a  B <  a
  //
  // with the branch on the left the same table read through the reversed operation. The shape
  // arises wherever a width taken as the COMMON width of two operands meets one of them again,
  // so a design that has to hold `x(W1) + y(W2)` in `W1` bits requires `W1 >= W2` and says so,
  // rather than restating the common width it went through.
  private object CompareAgainstMaxMin:
    private def mkBool(value: Boolean)(using dfc: DFC): ir.DFVal =
      import dfc.getSet
      ir.DFVal.Const(
        ir.DFBool, Some(value),
        dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags
      ).addMember

    // the same relation read from the other side
    private def reversed(op: FuncOp): FuncOp = op match
      case FuncOp.>= => FuncOp.<=
      case FuncOp.<= => FuncOp.>=
      case FuncOp.>  => FuncOp.<
      case FuncOp.<  => FuncOp.>
      case symmetric => symmetric // `===` and `=!=` read alike from either side

    // What is left of `chain` once the branch that is `self` is dropped, when `chain` is a
    // `maxMin` having it as a branch. `None` when it is not one, or does not.
    private def withoutBranch(chain: ir.DFVal, self: ir.DFVal, maxMin: FuncOp)(using
        dfc: DFC
    ): Option[ir.DFVal] =
      import dfc.getSet
      // ident-transparent, as the max/min chain absorption above is: either side may be a
      // (named) ident of the expression it stands for
      chain.stripTypePreservingAliases match
        case f: ir.DFVal.Func if f.dfType == ir.DFInt32 && f.op == maxMin =>
          val selfStripped = self.stripTypePreservingAliases
          val branches = f.args.map(_.get)
          val rest = branches.filterNot(_.stripTypePreservingAliases =~ selfStripped)
          if (rest.sizeIs == branches.size) None // `self` is not one of the branches
          else
            rest match
              // nothing but `self`, so the chain IS `self`; the chain absorption above is what
              // reduces that, and it does so before any comparison sees it
              case Nil         => None
              case only :: Nil => Some(only)
              case several     => Some(mkFunc(ir.DFInt32, maxMin, several))
        case _ => None
    end withoutBranch

    // the table above, for `maxMin(self, rest) op self`: an answer, or the operation to apply
    // between `rest` and `self`
    private def reduction(maxMin: FuncOp, op: FuncOp): Either[Boolean, FuncOp] =
      val isMax = maxMin == FuncOp.max
      op match
        case FuncOp.>=  => if (isMax) Left(true) else Right(FuncOp.>=)
        case FuncOp.<=  => if (isMax) Right(FuncOp.<=) else Left(true)
        case FuncOp.>   => if (isMax) Right(FuncOp.>) else Left(false)
        case FuncOp.<   => if (isMax) Left(false) else Right(FuncOp.<)
        case FuncOp.=== => if (isMax) Right(FuncOp.<=) else Right(FuncOp.>=)
        case _          => if (isMax) Right(FuncOp.>) else Right(FuncOp.<)

    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      opArgs match
        case (
              ir.DFBool,
              op @ (FuncOp.>= | FuncOp.<= | FuncOp.> | FuncOp.< | FuncOp.=== | FuncOp.=!=),
              List(lhs, rhs)
            ) =>
          // read with the chain on the left, which is the orientation the table is written in,
          // and put the answer back the way it was written
          def attempt(
              chain: ir.DFVal,
              self: ir.DFVal,
              chainOp: FuncOp,
              chainOnLeft: Boolean
          ): Option[ir.DFVal] =
            List(FuncOp.max, FuncOp.min).view.flatMap { maxMin =>
              withoutBranch(chain, self, maxMin).map { rest =>
                reduction(maxMin, chainOp) match
                  case Left(answer)  => mkBool(answer)
                  case Right(restOp) =>
                    if (chainOnLeft) mkFunc(ir.DFBool, restOp, List(rest, self))
                    else mkFunc(ir.DFBool, reversed(restOp), List(self, rest))
              }
            }.headOption
          attempt(lhs, rhs, op, chainOnLeft = true)
            .orElse(attempt(rhs, lhs, reversed(op), chainOnLeft = false))
        case _ => None
    end unapply
  end CompareAgainstMaxMin

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
              // `&`, `|` and `^` name TWO operations apiece: the binary bitwise/logical one
              // and the unary reduction (`a.^`, one operand, a single-bit result). A matching
              // `op` therefore does not imply a matching operation, and only the multi-operand
              // form of an associative op is associative at all. Absorbing across the two forms
              // splices a reduction's operand into a binary chain (or a binary chain's operands
              // into a reduction) and the reduction is simply lost: `a.^ ^ b.^` became
              // `a ^ b.^` and `(a ^ b).^` became `a ^ b` (issue #483).
              && rest.nonEmpty
              && prevFunc.args.sizeIs > 1
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
          // Purely additive: fresh refs for both the absorbed args and the appended tail args.
          // The absorbed Func's own arg refs are never reused (reuse entangles the two members'
          // tokens and breaks origin tracking), and the absorbed Func itself is never removed:
          // the front end may still hold a handle to it and reference it later (e.g. `lsbitsAt`
          // referencing its offset expression after the width computation absorbed it; issue
          // #449). When nothing ends up reading it, the end-of-design sweep drops it.
          val newArgRefs: List[ir.DFVal.Ref] = (prevFunc.args.map(_.get) ++ rest).map(
            _.refTW[ir.DFVal](knownReachable = true)
          )
          Some(
            ir.DFVal.Func(
              dfType, op, newArgRefs,
              dfc.ownerOrEmptyRef, meta, dfc.tags
            ).addMember
          )
        case _ => None
      end match
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
          // a fresh negated Const takes over the binding name; the original literal,
          // anonymized, becomes debris for the sweep (an anonymous member is never revised
          // in place; issue #449)
          const.asValAny.anonymizeInDFCPosition
          Some(
            ir.DFVal.Const(
              const.dfType,
              Some(-data),
              dfc.ownerOrEmptyRef,
              dfc.getMeta,
              dfc.tags
            ).addMember
          )
        case _ => None
      end match
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
        // x * 0 / 0 * x  ->  0 (a fresh Const; the operands become debris for the sweep)
        case (ir.DFInt32, FuncOp.`*`, List(_, c @ ir.DFVal.Const(data = Some(d: BigInt))))
            if d == BigInt(0) && c.isAnonymous =>
          Some(mkInt32Const(0))
        case (ir.DFInt32, FuncOp.`*`, List(c @ ir.DFVal.Const(data = Some(d: BigInt)), _))
            if d == BigInt(0) && c.isAnonymous =>
          Some(mkInt32Const(0))
        case _ => None
      end match
    end unapply
  end IdentityOps

  // Self-cancellation: two structurally equal operands.
  private object SelfCancelling:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        // a - a  ->  0 (ident-transparent: `W - a` cancels when `val W = <collapses to a>`)
        case (ir.DFInt32, FuncOp.-, List(a, b))
            if a.stripTypePreservingAliases =~ b.stripTypePreservingAliases =>
          Some(mkInt32Const(0))
        // max(a, a) / min(a, a)  ->  a
        case (ir.DFInt32, FuncOp.max | FuncOp.min, List(a, b))
            if a.stripTypePreservingAliases =~ b.stripTypePreservingAliases =>
          Some(rebindMeta(a))
        case _ => None
    end unapply
  end SelfCancelling

  // max/min between operands whose symbolic parts cancel, leaving a constant
  // difference (e.g., a value and itself plus/minus a constant offset):
  //   max(a, a + c) => (a + c) if c > 0, else a
  //   min(a, a + c) => a if c > 0, else (a + c)
  // Commutative in both operand orderings. The difference is decided by the
  // shared ir.IntExprCalc without resolving design params, so the picked
  // operand is the extremum under any parameter assignment.
  private object MaxMinWithOffset:
    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (ir.DFInt32, op @ (FuncOp.max | FuncOp.min), List(a, b)) =>
          ir.IntExprCalc.constDiff(a, b, resolveDesignParams = false).map { diff =>
            val aWins = if (op == FuncOp.max) diff >= 0 else diff <= 0
            rebindMeta(if (aWins) a else b)
          }
        case _ => None
      end match
    end unapply
  end MaxMinWithOffset

  // Cancels opposing +/- terms of the same non-constant DFVal across a DFInt32 additive
  // TREE. Handles e.g. `(x - 1) - x => -1` (which together with Const+Const folding handles
  // `x - 1 - x + 5 => 4`) and `x + (y - x) => y`, the shape a relative width adjustment
  // takes: `.eby(k)` asks for `sourceWidth + k`, and a `k` written as the distance to
  // another width states that width back.
  private object AdditiveCancellation:
    // The terms of the additive tree rooted at `v`, as (sign, DFVal). Descends through
    // ANONYMOUS `+`/`-` Funcs on EITHER side, associativity being no reason to prefer one:
    // the same relation is spelled left-nested by a chain of operations and right-nested by
    // one whose operand is a difference. A named Func is a value the user gave a name to and
    // stays one term, as does anything that is not an additive Func.
    private def collectTerms(v: ir.DFVal, sign: Int)(using
        ir.MemberGetSet
    ): List[(Int, ir.DFVal)] =
      v match
        case f: ir.DFVal.Func
            if f.isAnonymous && f.dfType == ir.DFInt32 &&
              (f.op == FuncOp.+ || f.op == FuncOp.-) && f.args.size == 2 =>
          val List(lhs, rhs) = f.args.map(_.get): @unchecked
          collectTerms(lhs, sign) ++ collectTerms(rhs, if (f.op == FuncOp.+) sign else -sign)
        case _ => List((sign, v))

    def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
      import dfc.getSet
      opArgs match
        case (ir.DFInt32, currentOp @ (FuncOp.+ | FuncOp.-), List(prev, curr)) =>
          val chain =
            collectTerms(prev, 1) ++ collectTerms(curr, if (currentOp == FuncOp.+) 1 else -1)
          if (chain.size < 2) None
          else
            // Find two terms with opposite signs whose DFVals are =~ (ident-transparent).
            val indexed = chain.zipWithIndex
            val pairOpt: Option[(Int, Int)] = indexed.iterator.collectFirst {
              case ((s1, t1), i) =>
                indexed.iterator.collectFirst {
                  case ((s2, t2), j)
                      if j != i && s1 == -s2 &&
                        t1.stripTypePreservingAliases =~ t2.stripTypePreservingAliases =>
                    (i, j)
                }
            }.flatten
            pairOpt.flatMap { case (i, j) =>
              val remaining = indexed.collect { case (term, k) if k != i && k != j => term }
              rebuildChain(remaining)
            }
          end if
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
