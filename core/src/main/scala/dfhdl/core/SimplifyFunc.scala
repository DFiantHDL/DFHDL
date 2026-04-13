package dfhdl.core
import dfhdl.compiler.ir
import DFVal.Func.Op as FuncOp
import dfhdl.internals.Position

private object SimplifyFunc:
  def unapply(opArgs: (ir.DFType, FuncOp, List[ir.DFVal]))(using dfc: DFC): Option[ir.DFVal] =
    import dfc.getSet
    opArgs match
      // Meta-programming can't safely touch the MutableDB, so all simplifications
      // are skipped in that mode.
      case _ if dfc.inMetaProgramming => None
      // Constant-fold DFInt32 add/sub with constant RHS when the LHS is itself
      // a same-shape anonymous Func. This MUST run before the multi-arg merge
      // below, otherwise `p3 + 1 + 1 + 1` would merge into a 4-arg Func instead
      // of folding into `p3 + 3`.
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
            if (newRHSData == BigInt(0))
              if (prevLHSArg.isAnonymous) Some(prevLHSArg.setMeta(_ => dfc.getMeta))
              else Some(prevLHSArg)
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
      // Merge consecutive same-op anonymous Funcs for associative operations.
      // E.g., `a + b + c` becomes Func(+, [a, b, c]) instead of nested binary Funcs.
      // For left-associative chains, only the first arg can be an absorbed Func.
      // Multi-referenced nodes (same Func used twice) are not merged.
      // Runs even in global context (no owner), unlike the other simplifications below.
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
      // TODO: maybe drop this limitation, if we can make DropStructsVecs work in
      // global context.
      case _ if dfc.ownerOption.isEmpty => None
      // special case to handle unary negation of anonymous decimal constants
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
    end match
  end unapply
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
end SimplifyFunc
