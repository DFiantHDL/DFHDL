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
  * anonymous non-carry `+`/`-`/`*` cone (or a `sel`, matching Verilog's `?:`) converted to a wider
  * type is re-evaluated at the target's width and sign. The warning helpers detect the
  * narrow-chain/implicit-`Int` patterns whose Verilog evaluation would diverge from DFHDL's
  * bit-accurate one; they are invoked from the `/`, `%`, comparison, and shift operation builders
  * in `DFDecimal` and `DFBits`.
  */
private[core] object CarryPromote:
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
    * re-enters the widening, while the condition passes through untouched.
    *
    * The candidate is taken BEFORE any sign conversion: an upstream anonymous sign-conversion alias
    * (the commutative-arith sign alignment creates one) is unwrapped, or it would hide the func and
    * pin the chain at its narrow width. A carry func (result wider than its operands) keeps its
    * documented exact semantics and converts as a leaf; so do all other ops (shifts, bitwise,
    * comparisons), whose evaluation this rule does not context-widen.
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

    // symbolic elimination keeps this consistent with the width-fit acceptance
    // rule of the TC conversion: `16 > WIDTH max 16` decides as `16 > 16` (no
    // widening), so the anonymous form resolves exactly like a named
    // intermediate value; if still undecidable, optimistically assume the
    // target is wider.
    def contextWidenCheck(funcWidth: IntParam[Int]): Boolean =
      dfType.asFE[DFSInt[Int]]
        .compareWidths(DFXInt(true, funcWidth, BitAccurate), elimSymbolicMaxMin = true)(
          _ > _
        )
        .getOrElse(true)

    // The widened Func is BUILT FRESH rather than revised in place (an anonymous
    // member is never revised; issue #449); the original cone becomes debris for
    // the end-of-design sweep. The spelling of the result (a carry op or explicit
    // operand widenings) is purely a PRINTING decision, reconstructed from this
    // shape by the CarryFunc/Eby extractors. The widened evaluation type is the
    // target itself as a bit-accurate type; an Int target widens the cone at its
    // native 32-bit width (Verilog's `integer` context) and converts by the caller.
    def newDT = dfType.asIR.asInstanceOf[ir.DFDecimal].copy(
      magnitudeWidthParamRef = dfType.widthIntParam.ref,
      nativeType = BitAccurate
    )
    // an argument re-enters the full conversion, so nested cones widen and leaves
    // get their sign conversion / resize at the target type
    def widenedArg(argRef: ir.DFVal.Ref): DFValAny =
      DFXInt.Val.Ops.toDFXIntOf(
        argRef.get.asValOf[DFXInt[Boolean, Int, NativeType]]
      )(DFXInt(dfType.signed, dfType.widthIntParam, BitAccurate))(using
        dfc.anonymize
      )
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
            op = FuncOp.+ | FuncOp.- | FuncOp.*
          )
          if func.isAnonymous && {
            // non-carry (modular) func: its type equals its aligned operands'
            func.dfType =~ func.args.head.get.dfType &&
            contextWidenCheck(func.asValOf[DFSInt[Int]].widthIntParam)
          } =>
        Some(rebuilt(func, func.args.map(widenedArg(_).asIR)))
      case func @ ir.DFVal.Func(
            dfType = ir.DFUInt(_) | ir.DFSInt(_),
            op = FuncOp.sel
          )
          // a sel's type structurally equals both branches' types (the frontend
          // converts one branch to the other's type), so no operand-shape gate
          if func.isAnonymous &&
            contextWidenCheck(func.asValOf[DFSInt[Int]].widthIntParam) =>
        Some(rebuilt(func, func.args.head.get :: func.args.tail.map(widenedArg(_).asIR)))
      case _ => None
    end match
  end widenedOpt

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
  )(using ir.MemberGetSet): Boolean =
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
              case _ => false

  // Check if an anonymous sub-tree contains narrow non-carry arith that
  // also has an ImplicitlyFromIntTag operand (Verilog "Forcing Larger
  // Evaluation" pattern).
  private[core] def containsNarrowNonCarryArithWithTaggedOperand(
      dfVal: ir.DFVal
  )(using ir.MemberGetSet): Boolean =
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
              case _ => false

  // Unified Verilog-semantics warning trigger shared by `/`, `%` (arithOp)
  // and comparison operations (DFXIntCompare). Warns when a narrow non-carry
  // chain mixes with a tagged-from-Int operand on either side - directly OR
  // nested inside the chain.
  private[core] def shouldWarnVerilogSemantics(
      lhs: ir.DFVal,
      rhs: ir.DFVal
  )(using ir.MemberGetSet): Boolean =
    (hasImplicitlyFromIntTag(rhs) && containsNarrowNonCarryArith(lhs)) ||
      (hasImplicitlyFromIntTag(lhs) && containsNarrowNonCarryArith(rhs)) ||
      containsNarrowNonCarryArithWithTaggedOperand(lhs) ||
      containsNarrowNonCarryArithWithTaggedOperand(rhs)
end CarryPromote
