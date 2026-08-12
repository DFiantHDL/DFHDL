package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.*
import DFDecimal.Extensions.*

/** Target-context widening ("carry promotion") of anonymous integer expressions, and the related
  * Verilog-semantics warning machinery.
  *
  * [[widenedOpt]] holds the deep re-evaluation rule used by the `toDFXIntOf` conversion: an
  * anonymous non-carry `+`/`-`/`*` cone (or a `sel`, matching Verilog's `?:`, or an `if`/`match`
  * expression, matching the per-branch assignments it lowers to) converted to a wider type is
  * re-evaluated at the target's width and sign. The warning helpers detect the
  * narrow-chain/implicit-`Int` patterns whose Verilog evaluation would diverge from DFHDL's
  * bit-accurate one; they are invoked from the `/`, `%`, comparison, and shift operation builders
  * in `DFDecimal` and `DFBits`.
  */
private[core] object CarryPromote:
  /** The total-width ref of an integer type: its fraction is 0, so the magnitude ref IS the width
    * ref. `None` for any other type, which never widens.
    */
  private def widthRefOpt(dfTypeIR: ir.DFType): Option[ir.IntParamRef] =
    dfTypeIR match
      case dec: ir.DFDecimal => Some(dec.magnitudeWidthParamRef)
      case _                 => None

  /** Deep target-context widening, matching Verilog's assignment-context width propagation: an
    * anonymous non-carry `+`/`-`/`*` cone converted to a WIDER type is re-evaluated at the target's
    * width and sign. Every func in the cone is retyped to the target and every leaf is converted to
    * it (recursively, via `toDFXIntOf` on each argument), so all intermediates evaluate at the
    * target width. Truncation to the target width commutes with `+`/`-`/`*`, so this is the unique
    * evaluation that agrees with Verilog for every input; in particular a sign conversion is
    * applied to the OPERANDS, never to a narrower result (zero-extending a wrapped subtraction
    * result flips its sign).
    *
    * A `sel` is context-transparent the same way: it corresponds to Verilog's `?:`, whose branch
    * operands are context-determined, so the selection re-types to the target and each branch
    * re-enters the widening, while the condition passes through untouched. An `if`/`match`
    * EXPRESSION is likewise transparent, matching the per-branch assignments it lowers to; its
    * blocks are revised in place (see the conditional-header case below for the mechanics). A
    * shift's LEFT operand is context-determined too (the amount is self-determined), gated on the
    * target keeping the operand's signedness (see the shift case below).
    *
    * The candidate is taken BEFORE any sign conversion: an upstream anonymous sign-conversion alias
    * (the commutative-arith sign alignment creates one) is unwrapped, or it would hide the func and
    * pin the chain at its narrow width. A carry func (result wider than its operands) keeps its
    * documented exact semantics and converts as a leaf; so do all other ops (bitwise logic,
    * comparisons, rotations), whose evaluation this rule does not context-widen.
    *
    * Returns `None` when no widening applies, leaving the plain leaf conversion to the caller
    * (`toDFXIntOf` in `DFDecimal`).
    */
  private[core] def widenedOpt[RS <: Boolean, RW <: IntP, RN <: NativeType](
      lhsIR: ir.DFVal,
      dfType: DFXInt[RS, RW, RN]
  )(using dfc: DFC): Option[DFValOf[DFSInt[Int]]] =
    import dfc.getSet
    val candidateIR = signConversionRelVal(lhsIR).getOrElse(lhsIR)

    // The target must be strictly wider than the value's own type for the widening to
    // apply. Decided directly on the two IR width refs: constructing a DFHDL type as a
    // width carrier would run that type's own width constraint, so a 1-bit cone would
    // fail `SInt`'s "width must be larger than 1" rule (issue #476).
    //
    // Symbolic elimination keeps this consistent with the width-fit acceptance rule of
    // the TC conversion: `16 > WIDTH max 16` decides as `16 > 16` (no widening), so the
    // anonymous form resolves exactly like a named intermediate value; if still
    // undecidable, optimistically assume the target is wider.
    def contextWidenCheck(valDFType: ir.DFType): Boolean =
      widthRefOpt(valDFType).exists { valWidthRef =>
        dfType.asIR.magnitudeWidthParamRef
          .compare(valWidthRef, elimSymbolicMaxMin = true)(_ > _)
          .getOrElse(true)
      }

    // The widened Func is BUILT FRESH rather than revised in place (an anonymous
    // member is never revised; issue #449); the original cone becomes debris for
    // the end-of-design sweep. The spelling of the result (a carry op or explicit
    // operand widenings) is purely a PRINTING decision, reconstructed from this
    // shape by the CarryFunc/Eby extractors. The widened evaluation type is the
    // target itself as a bit-accurate type; an Int target widens the cone at its
    // native 32-bit width (Verilog's `integer` context) and converts by the caller.
    def newDT = dfType.asIR.copy(
      magnitudeWidthParamRef = dfType.widthIntParam.ref,
      nativeType = BitAccurate
    )
    def targetType = DFXInt(dfType.signed, dfType.widthIntParam, BitAccurate)
    // a nested value re-enters the full conversion, so nested cones widen and leaves
    // get their sign conversion / resize at the target type
    def widened(v: ir.DFVal): DFValAny =
      wildcardUnder(v) match
        // A wildcard `Int` operand adapted to the OTHER operand's width: the wildcard
        // re-adapts to the target, rather than its adaptation being widened. The other
        // operand's width is precisely what the target context replaces, so evaluating
        // the wildcard at it first is the narrow evaluation this rule exists to undo:
        // it truncates a literal the target holds perfectly well, and, where that width
        // is parametric, leaves the design constrained to hold a value nothing in the
        // widened expression puts there. The fit at the TARGET is checked in its place.
        case Some(wildcard) =>
          AutoConstraint.retract(v)
          DFXInt.Val.Ops.adaptWildcard(wildcard.asValAny, targetType)(using dfc.anonymize)
        case None =>
          DFXInt.Val.Ops.toDFXIntOf(
            v.asValOf[DFXInt[Boolean, Int, NativeType]]
          )(targetType)(using dfc.anonymize)
    def widenedArg(argRef: ir.DFVal.Ref): DFValAny = widened(argRef.get)
    // no MutableDB revision under meta-programming (matching `setMember`'s behavior
    // there): the retyped value is returned unregistered and the argument
    // conversions are skipped, since no member is registered
    def rebuilt(func: ir.DFVal.Func, newArgs: => List[ir.DFVal]): DFValOf[DFSInt[Int]] =
      if (dfc.inMetaProgramming) func.updateDFType(newDT).asValOf[DFSInt[Int]]
      else
        ir.DFVal.Func(
          newDT,
          func.op,
          newArgs.map(_.refTW[ir.DFVal](knownReachable = true)),
          dfc.ownerOrEmptyRef,
          func.meta,
          func.tags
        ).addMember.asValOf[DFSInt[Int]]

    candidateIR match
      case func @ ir.DFVal.Func(
            dfType = ir.DFUInt(_) | ir.DFSInt(_),
            op = FuncOp.+ | FuncOp.- | FuncOp.* | FuncOp.unary_-
          )
          if func.isAnonymous && {
            // non-carry (modular) func: its type equals its aligned operands'
            func.dfType =~ func.args.head.get.dfType &&
            contextWidenCheck(func.dfType)
          } =>
        Some(rebuilt(func, func.args.map(widenedArg(_).asIR)))
      // A shift's LEFT operand is context-determined in Verilog (the amount is
      // self-determined), so an anonymous shift converted to a wider SAME-SIGN type
      // re-types to the target and its left operand re-enters the widening: the high
      // bits a narrow evaluation would lose (`>>` bringing down a carry bit, `<<`
      // pushing into the extension range) are exactly what the context preserves. A
      // sign-CROSSING shift context stays a leaf: a shift evaluates at its operand's
      // own signedness (an arithmetic-vs-logical `>>` difference), so the sign
      // conversion cannot move to the operands; the explicit spelling states the
      // intent there.
      case func @ ir.DFVal.Func(
            dfType = ir.DFDecimal(funcSigned, _, 0, BitAccurate),
            op = FuncOp.>> | FuncOp.<<
          )
          if func.isAnonymous && funcSigned == dfType.asIR.signed &&
            contextWidenCheck(func.dfType) =>
        Some(rebuilt(func, widenedArg(func.args.head).asIR :: func.args.tail.map(_.get)))
      case func @ ir.DFVal.Func(
            dfType = ir.DFUInt(_) | ir.DFSInt(_),
            op = FuncOp.sel
          )
          // a sel's type structurally equals both branches' types (the frontend
          // converts one branch to the other's type), so no operand-shape gate
          if func.isAnonymous &&
            contextWidenCheck(func.dfType) =>
        Some(rebuilt(func, func.args.head.get :: func.args.tail.map(widenedArg(_).asIR)))
      // A conditional EXPRESSION (if/match) re-evaluates each branch at the target,
      // matching the Verilog its branches lower to (per-branch assignments to the
      // wider target). The type-driven construction (fromBranchesExact1/fromCasesExact)
      // already converts inside the branches; this covers the type-free positions
      // (an operand of a wider operation, a connection RHS), where the header was
      // typed by its branches. Each branch's terminal ident is superseded by a fresh
      // ident over the branch value's widened re-evaluation, built INSIDE the branch
      // block (so branch-local named values stay in scope); the old terminal and cone
      // become debris for the end-of-design sweep, and the header is revised in place
      // to the target type, the same revision its construction applies.
      case header: ir.DFConditional.Header
          if header.isAnonymous &&
            (header.dfType match
              case ir.DFUInt(_) | ir.DFSInt(_) => true
              case _ => false) && contextWidenCheck(header.dfType) =>
        if (dfc.inMetaProgramming) Some(header.updateDFType(newDT).asValOf[DFSInt[Int]])
        else
          // all-or-nothing: an unexpected branch shape (no terminal ident) leaves the
          // whole conversion to the caller's leaf path
          condBranchTerminals(header).map { branchVals =>
            branchVals.foreach { (block, oldIdent, branchVal) =>
              // the widened members are INSERTED after the old terminal, inside the
              // block's span, keeping the flat member list properly nested
              dfc.mutableDB.insertingAfter(oldIdent) {
                dfc.enterOwner(block.asFE)
                DFVal.Alias.AsIs.ident(widened(branchVal))(using dfc.anonymize)
                dfc.exitOwner()
              }
              // the superseded terminal is dropped explicitly: an ident is consumed
              // positionally (never by reference), so the sweep alone would keep it
              // and, through it, the superseded narrow cone
              dfc.mutableDB.ignoreMember(oldIdent)
            }
            header.replaceMemberWith(header.updateDFType(newDT)).asValOf[DFSInt[Int]]
          }
      case _ => None
    end match
  end widenedOpt

  // The branch blocks, terminal idents, and terminal values of a conditional
  // EXPRESSION, recovered from the raw creation-ordered member list of the current
  // design context via plain ref walks. This runs MID-construction (the enclosing
  // statement is still being built), where a designDB flat snapshot (`members`,
  // `getCBList`) is unavailable: its owner-member generation requires the closed,
  // properly-nested state. Each block's terminal is its LAST directly-owned value
  // (nested constructs in a branch body own their internals, so they never shadow
  // the terminal ident). Returns None when any branch lacks a terminal ident (an
  // unexpected shape).
  private def condBranchTerminals(header: ir.DFConditional.Header)(using
      dfc: DFC
  ): Option[List[(ir.DFConditional.Block, ir.DFVal, ir.DFVal)]] =
    import dfc.getSet
    import dfhdl.compiler.analysis.{getHeaderCB, Ident}
    val memberList = dfc.mutableDB.DesignContext.current.getImmutableMemberList
    val blocks = memberList.collect {
      case cb: ir.DFConditional.Block if cb.getHeaderCB == header => cb
    }
    val blockSet = blocks.toSet
    val lastOwnedByBlock =
      memberList.foldLeft(Map.empty[ir.DFConditional.Block, ir.DFVal]) { (acc, m) =>
        m match
          case v: ir.DFVal =>
            v.ownerRef.get match
              case cb: ir.DFConditional.Block if blockSet(cb) => acc.updated(cb, v)
              case _                                          => acc
          case _ => acc
      }
    val branchVals = blocks.flatMap { block =>
      lastOwnedByBlock.get(block).collect {
        case ident @ Ident(underlying) => (block, ident, underlying)
      }
    }
    Option.when(branchVals.sizeCompare(blocks) == 0)(branchVals)
  end condBranchTerminals

  private[core] val verilogSemanticsWarnMsg =
    """|Implicit Scala/DFHDL Int conversion may produce different results than Verilog.
       |In Verilog, integer literals are 32-bit, which can widen intermediate arithmetic.
       |In DFHDL, Int literals are converted to minimum bit-accurate width.
       |Use carry operations (+^, -^, *^) or explicit bit-accurate literals (d"W'V").""".stripMargin

  // Check if a value is tagged with ImplicitlyFromIntTag. An implicit `Int` operand
  // adapted to a parametric width keeps its tagged const under a resize alias (the
  // fold into a single const happens only for literal widths), so the check follows
  // alias chains down to the underlying value.
  private[core] def hasImplicitlyFromIntTag(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean =
    dfVal.tags.hasTagOf[ir.ImplicitlyFromIntTag] ||
      (dfVal match
        case alias: ir.DFVal.Alias => hasImplicitlyFromIntTag(alias.relValRef.get)
        case _                     => false)

  // The wildcard `Int` under a value that is nothing but that wildcard adapted to some other
  // operand's width: an anonymous alias chain, as `toDFXIntOf` builds it, bottoming out either
  // at the tagged constant a Scala `Int`'s candidate created (at the value's own minimum width,
  // so a sign conversion and a resize may sit above it) or at a DFHDL `Int`, which has no width
  // at all and takes one in a single conversion. `None` for everything else, the wildcard's own
  // constant included: there is no adaptation there to look through.
  private def wildcardUnder(dfVal: ir.DFVal)(using ir.MemberGetSet): Option[ir.DFVal] =
    dfVal match
      case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
        val relVal = alias.relValRef.get
        val isWildcard = relVal.tags.hasTagOf[ir.ImplicitlyFromIntTag] ||
          relVal.dfType == ir.DFInt32
        if (isWildcard) Some(relVal) else wildcardUnder(relVal)
      case _ => None

  // A width reference resolved through design parameters: this runs during
  // elaboration, where a parameter's applied (or default) value is known, so a
  // parametric width like `CORDW + 1` resolves to its actual value.
  private def resolvedWidthOf(ref: ir.IntParamRef)(using
      getSet: ir.MemberGetSet
  ): Option[Int] =
    ref.getIntConstData(using
      getSet,
      ir.ConstData.CachePolicy.GoThroughDesignParams
    ) match
      case ir.ConstData.KnownConst(w) => Some(w)
      case _                          => None

  // A value's width classified as narrow (< 32 bits). A width that cannot be
  // resolved counts as narrow: a false-positive warning costs one carry op, while a
  // false negative is silently wrong hardware.
  private def resolvedWidthIsNarrow(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean =
    dfVal.dfType match
      case dec: ir.DFDecimal =>
        resolvedWidthOf(dec.magnitudeWidthParamRef) match
          case Some(m) => m + dec.fractionWidth < 32
          case None    => true
      case _ =>
        dfVal.dfType.widthIntOpt.map(_ < 32).getOrElse(true)

  // An anonymous sign-conversion alias: an unsigned value reinterpreted as signed
  // with exactly one extra bit (`.signed`). The Verilog backend emits it as
  // `$signed({1'b0, ...})`, whose concatenation operand is self-determined, so a
  // narrow chain stays narrow through it and the promotion/warning machinery must
  // look through it. An equal-width alias is a reinterpret cast and never matches.
  private def signConversionRelVal(dfVal: ir.DFVal)(using
      ir.MemberGetSet
  ): Option[ir.DFVal] =
    dfVal match
      case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
        alias.dfType match
          case ir.DFSInt(aliasWidthRef) =>
            val relVal = alias.relValRef.get
            relVal.dfType match
              case ir.DFUInt(relWidthRef) =>
                (resolvedWidthOf(aliasWidthRef), resolvedWidthOf(relWidthRef)) match
                  case (Some(aw), Some(rw)) if aw == rw + 1 => Some(relVal)
                  case _                                    => None
              case _ => None
          case _ => None
      case _ => None

  // Check if an anonymous sub-tree contains non-carry +/-/* with width < 32.
  private[core] def containsNarrowNonCarryArith(
      dfVal: ir.DFVal
  )(using dfc: DFC): Boolean =
    import dfc.getSet
    dfVal match
      case func: ir.DFVal.Func if func.isAnonymous =>
        func.op match
          case FuncOp.+ | FuncOp.- | FuncOp.* =>
            // carry-ness is a SHAPE property now (operand-widened funcs, see CarryFunc)
            val isNonCarry =
              dfhdl.compiler.analysis.CarryFunc.unapply(func).isEmpty
            val isNarrowNonCarry = isNonCarry && resolvedWidthIsNarrow(func)
            isNarrowNonCarry ||
            func.args.exists(ref => containsNarrowNonCarryArith(ref.get))
          case _ =>
            func.args.exists(ref => containsNarrowNonCarryArith(ref.get))
      case _ =>
        signConversionRelVal(dfVal) match
          case Some(relVal) => containsNarrowNonCarryArith(relVal)
          case None         =>
            dfVal match
              case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
                dfhdl.compiler.analysis.Eby.unapply(alias) match
                  case Some(relVal, _) => containsNarrowNonCarryArith(relVal)
                  case None            => false
              // a conditional EXPRESSION hides a chain one selection away: each
              // branch terminal is an operand position too (issue #464 warning gap)
              case header: ir.DFConditional.Header if header.isAnonymous =>
                condBranchTerminals(header)
                  .exists(_.exists((_, _, v) => containsNarrowNonCarryArith(v)))
              case _ => false
    end match
  end containsNarrowNonCarryArith

  // Check if an anonymous sub-tree contains narrow non-carry arith that
  // also has an ImplicitlyFromIntTag operand (Verilog "Forcing Larger
  // Evaluation" pattern).
  private[core] def containsNarrowNonCarryArithWithTaggedOperand(
      dfVal: ir.DFVal
  )(using dfc: DFC): Boolean =
    import dfc.getSet
    dfVal match
      case func: ir.DFVal.Func if func.isAnonymous =>
        func.op match
          case FuncOp.+ | FuncOp.- | FuncOp.* =>
            val isNonCarry =
              dfhdl.compiler.analysis.CarryFunc.unapply(func).isEmpty
            val isNarrowNonCarry = isNonCarry && resolvedWidthIsNarrow(func)
            (isNarrowNonCarry && func.args.exists(ref => hasImplicitlyFromIntTag(ref.get))) ||
            func.args.exists(ref =>
              containsNarrowNonCarryArithWithTaggedOperand(ref.get)
            )
          case _ =>
            func.args.exists(ref =>
              containsNarrowNonCarryArithWithTaggedOperand(ref.get)
            )
      case _ =>
        signConversionRelVal(dfVal) match
          case Some(relVal) => containsNarrowNonCarryArithWithTaggedOperand(relVal)
          case None         =>
            dfVal match
              case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
                dfhdl.compiler.analysis.Eby.unapply(alias) match
                  case Some(relVal, _) =>
                    containsNarrowNonCarryArithWithTaggedOperand(relVal)
                  case None => false
              // a conditional EXPRESSION hides a chain one selection away: each
              // branch terminal is an operand position too (issue #464 warning gap)
              case header: ir.DFConditional.Header if header.isAnonymous =>
                condBranchTerminals(header)
                  .exists(_.exists((_, _, v) => containsNarrowNonCarryArithWithTaggedOperand(v)))
              case _ => false
    end match
  end containsNarrowNonCarryArithWithTaggedOperand

  // Unified Verilog-semantics warning trigger shared by `/`, `%` (arithOp)
  // and comparison operations (DFXIntCompare). Warns when a narrow non-carry
  // chain mixes with a tagged-from-Int operand on either side - directly OR
  // nested inside the chain.
  private[core] def shouldWarnVerilogSemantics(
      lhs: ir.DFVal,
      rhs: ir.DFVal
  )(using DFC): Boolean =
    import dfc.getSet
    (hasImplicitlyFromIntTag(rhs) && containsNarrowNonCarryArith(lhs)) ||
    (hasImplicitlyFromIntTag(lhs) && containsNarrowNonCarryArith(rhs)) ||
    containsNarrowNonCarryArithWithTaggedOperand(lhs) ||
    containsNarrowNonCarryArithWithTaggedOperand(rhs)
end CarryPromote
