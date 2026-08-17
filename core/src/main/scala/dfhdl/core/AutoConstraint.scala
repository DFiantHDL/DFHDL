package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.TextOut.Severity
import scala.collection.mutable

/** The assumptions a design's elaboration made but could not prove, and the static assertions they
  * become.
  *
  * The width algebra decides a parametric relation three ways: provably true (nothing to do),
  * provably false (an elaboration error), and undecidable. An undecidable relation that an
  * operation nevertheless RELIES ON is an assumption, and it is neither honest to reject the
  * operation for it nor to accept it silently. The third way is to accept the operation and state
  * the assumption in the design, so that every instantiation checks at its own elaboration what
  * this one could not.
  *
  * A check that cannot decide calls [[raise]] with the condition it assumed. That condition is a
  * constant expression over the design's parameters; it is tagged [[ir.AutoConstraint]] and left
  * exactly where the check built it. The end of the design body ([[materialize]]) collects the
  * tagged conditions, drops the ones stating a relation another already states, and plants one
  * static assertion per survivor at the tail of the body.
  *
  * What the assertion reports is derived from the condition and nothing else. The check that raised
  * it has a message of its own, but that message describes ONE operation, while the assertion
  * describes the design's interface: it survives dedup and minimization, which merge the
  * assumptions of several operations into one statement, and it is read by whoever instantiates the
  * generated module rather than by whoever wrote the assignment. The operation's own message stays
  * where it belongs, on the elaboration error for a relation that is provably violated.
  */
object AutoConstraint:
  /** The name a materialized constraint carries. It labels the statement in the generated HDL,
    * where an unnamed SystemVerilog elaboration block is what a linter complains about. Enumerated
    * HERE rather than left to `UniqueNames`, because the printed DFHDL is source: two `val
    * constraint = ...` bindings in one body would not re-elaborate. The enumeration follows
    * `UniqueNames`'s own, so it renames nothing further, and it runs from the first constraint even
    * when a design has only one: the bare `constraint` is a SystemVerilog keyword, which the
    * generated elaboration block cannot be labelled with.
    */
  private def constraintName(idx: Int, count: Int): String =
    s"constraint_${idx.toPaddedString(count)}"

  /** A constraint's condition. Constant, so the assertion it becomes is a contract checked at the
    * generated design's elaboration rather than a runtime test.
    */
  type Guard = DFValOf[DFBool]

  private def condition(op: FuncOp, lhs: IntParam[Int], rhs: IntParam[Int])(using
      dfc: DFC
  ): Guard =
    given DFC = dfc.anonymize
    DFVal.Func[DFBool, Any](DFBool, op, List(lhs.toDFConst.asIR, rhs.toDFConst.asIR))

  /** The condition `lhs >= rhs`, built as a value instead of decided. */
  def ge(lhs: IntParam[Int], rhs: IntParam[Int])(using DFC): Guard =
    condition(FuncOp.>=, lhs, rhs)

  /** Both conditions, for a requirement that takes more than one relation to state. It stays ONE
    * constraint: the parts are what the design must satisfy together, and reporting a violation of
    * one without showing the other names half a contract.
    */
  def and(lhs: Guard, rhs: Guard)(using dfc: DFC): Guard =
    given DFC = dfc.anonymize
    DFVal.Func[DFBool, Any](DFBool, FuncOp.&, List(lhs.asIR, rhs.asIR))

  /** Decides the width fit `lhs >= rhs`, or `None` when it holds for some parameter assignments and
    * not others. The undecided answer is what [[raise]] exists for.
    */
  def widthFitGE(lhs: IntParam[Int], rhs: IntParam[Int])(using dfc: DFC): Option[Boolean] =
    import dfc.getSet
    // decided on the values rather than on references to them: a reference minted here belongs to
    // no member, and a reference with no origin member is one the printer cannot resolve a
    // relative name against (see `IntParam.errorString`)
    (lhs.toScalaIntOpt, rhs.toScalaIntOpt) match
      case (Some(lhsInt), Some(rhsInt)) => Some(lhsInt >= rhsInt)
      case _ => ir.IntExprCalc.widthFitCompare(lhs.toDFConst.asIR, rhs.toDFConst.asIR)

  /** Records `guard` as an assumption of the design being elaborated, to be materialized as a
    * static assertion at the end of its body. Answers the guard as recorded, for [[raiseFor]] to
    * key on, and `None` where nothing was recorded.
    *
    * Nothing is recorded anywhere else: the guard IS the record, and its own meta is the position
    * of the operation that assumed it.
    */
  def raise(guard: Guard)(using dfc: DFC): Option[ir.DFVal] =
    import dfc.getSet
    // A condition can fold to a constant on its way here, a relation between a `max`/`min` and
    // one of its own branches being decidable by simplification where the width proof cannot
    // decide it. One that folded to `true` requires nothing of the design and states nothing;
    // one that folded to `false` is kept, an assumption that cannot hold being worth the noise.
    val decided = guard.asIR.getConstData[Option[Boolean]] match
      case ir.ConstData.KnownConst(Some(true)) => true
      case _                                   => false
    // nothing states a constraint outside a design: global scope has no body to put it in, and a
    // stage's meta design transforms an already-elaborated one and assumes nothing of its own
    if (!decided && !dfc.inMetaProgramming && dfc.ownerOption.isDefined)
      Some(guard.asIR.setTags(_.tag(ir.AutoConstraint)))
    else None

  /** [[raise]]s `lhs >= rhs`, unless the relation is decided either way, which is the same
    * three-way every width relation takes: a decided one either holds for every parameter
    * assignment or was the caller's to reject, and neither is an assumption to state.
    */
  def raiseUndecidedFit(lhs: IntParam[Int], rhs: IntParam[Int])(using DFC): Unit =
    if (widthFitGE(lhs, rhs).isEmpty) raise(ge(lhs, rhs))
    ()

  /** [[raise]], for an assumption a specific VALUE makes: the constraint stands as long as that
    * value does, and is [[retract]]ed when something supersedes it.
    *
    * An assumption is normally the design's for good, because the operation that made it is a
    * statement of the body. An anonymous operand is not: target-context widening re-evaluates a
    * whole expression at the target's width (see `CarryPromote.widenedOpt`), which discards the
    * narrow form of every operand in it, and an assumption only that narrow form needed has to go
    * with it rather than be left stating a requirement of the design nothing in it relies on.
    */
  def raiseFor(guard: Guard, subject: ir.DFVal)(using dfc: DFC): Unit =
    raise(guard).foreach(dfc.mutableDB.DesignContext.current.autoConstraintOf += subject -> _)

  /** Drops the assumption [[raiseFor]] recorded for `subject`, if any. The guard is left where it
    * is, untagged: it is then read by nothing, and the end-of-design sweep collects it along with
    * the superseded value itself.
    */
  def retract(subject: ir.DFVal)(using dfc: DFC): Unit =
    import dfc.getSet
    dfc.mutableDB.DesignContext.current.autoConstraintOf.remove(subject).foreach { guard =>
      guard.setTags(_.removeTagOf[ir.AutoConstraint])
      ()
    }

  /** [[raise]]s `param == <value>` for every parameter of the design whose DATA its elaboration
    * reads (`DFVal.DesignParam.isDataImpure`).
    *
    * Reading a parameter is what specializes a body to it: the `if` branch that was not taken and
    * the `for` iteration that did not run leave nothing behind, so the design that comes out is the
    * one for that value and no other. Elaboration has always done this silently, and the generated
    * module went on exposing the parameter as overridable; the assertion is what makes the
    * specialization something the design says rather than something it merely is. It is the same
    * fact the elaboration cache already keys on, said out loud.
    *
    * Nothing is stated where an overriding instantiation cannot exist. A method design is
    * instantiated per call site, by DFHDL, with the value it was keyed on; a blackbox has no body
    * that read anything. Nor for a parametrically-typed parameter (`INIT: Bits[W] <> CONST`), whose
    * value would have to be compared against a literal of that same parametric type, which no
    * constant can be built at; such a parameter is also one the width algebra never folds, so only
    * the contract is left unsaid.
    */
  private def raiseDataImpureParams(ctx: DesignContext)(using dfc: DFC): Unit =
    import dfc.getSet
    val design = dfc.owner.asIR.getThisOrOwnerDesign
    val names = design.dataImpureParamNames
    if (names.nonEmpty && !design.isBlackBox && design.instMode != ir.DFDesignBlock.InstMode.Def)
      ctx.getImmutableMemberList.foreach {
        case dp: ir.DFVal.DesignParam
            if (dp.getOwnerDesign eq design) && dp.isDataImpure &&
              dp.dfType.getRefs.isEmpty =>
          dp.getConstDataThroughParams[ir.Data].foreach { data =>
            given DFC = dfc.anonymize
            val value = DFVal.Const.forced(dp.dfType.asFE[DFTypeAny], data)
            raise(DFVal.Func[DFBool, Any](DFBool, FuncOp.===, List(dp, value.asIR)))
            ()
          }
        case _ =>
      }
  end raiseDataImpureParams

  /** Whether `value` carries any width-adjustment permission at all, in either direction. */
  def hasWidthAdjustPermission(value: DFValAny)(using DFC): Boolean =
    value.hasTag[ir.ResizeTag] || value.hasTag[ir.ExtendTag] || value.hasTag[ir.TruncateTag]

  /** Carries `from`'s width-adjustment permission, if it has one, onto `to`.
    *
    * A permission is about the VALUE's width, and converting between `Bits` and an integer type
    * leaves that width alone, so the permission has to survive the conversion. Dropping it makes
    * the diagnostic absurd rather than merely unhelpful: `b8 := u4.extend` would report the width
    * mismatch and recommend the `.extend` that is already written.
    */
  def carryWidthAdjustPermission[T <: DFTypeAny, M <: ModifierAny](
      from: DFValAny,
      to: DFVal[T, M]
  )(using DFC): DFVal[T, M] =
    if (from.hasTag[ir.ResizeTag]) to.tag(ir.ResizeTag)
    else if (from.hasTag[ir.ExtendTag]) to.tag(ir.ExtendTag)
    else if (from.hasTag[ir.TruncateTag]) to.tag(ir.TruncateTag)
    else to

  /** Whether a width-adjustment permission carried by `value` (see `ir.ExtendTag`) covers adjusting
    * it to `targetWidth`.
    *
    * A permission covers ONE direction. Where it does not apply it answers false, contributing
    * nothing, and the caller's own width rule decides and reports, exactly as it would for an
    * untagged value: a permission is never a claim that an adjustment happens, only that one may.
    *
    * An undecidable pair is covered, with the relation the permission relies on stated as a
    * constraint of the design, on the same three-way as every other width decision made over
    * parameters (see [[widthFitGE]]).
    */
  def permitsWidthAdjust(value: DFValAny, targetWidth: IntParam[Int])(using dfc: DFC): Boolean =
    import dfc.getSet
    def covers(wider: IntParam[Int], narrower: IntParam[Int]): Boolean =
      widthFitGE(wider, narrower) match
        case Some(decided) => decided
        case None          =>
          raise(ge(wider, narrower))
          true
    // only the types that carry these permissions are answered for; an integer decimal keeps its
    // total width in the magnitude ref (fraction 0)
    val sourceWidthOpt: Option[IntParam[Int]] = value.dfType.asIR match
      case dt: ir.DFBitsWL                           => Some(dt.widthParamRef.get)
      case dt: ir.DFDecimal if dt.fractionWidth == 0 => Some(dt.magnitudeWidthParamRef.get)
      case _                                         => None
    if (value.hasTag[ir.ResizeTag]) true
    else if (value.hasTag[ir.ExtendTag])
      sourceWidthOpt.exists(covers(targetWidth, _))
    else if (value.hasTag[ir.TruncateTag])
      sourceWidthOpt.exists(covers(_, targetWidth))
    else false
  end permitsWidthAdjust

  /** What a guard requires, canonically: the relation it states, as `linear >= 0`. */
  private type Requirement = ir.IntExprCalc.Linear

  /** What a guard requires, as the conjunction of one or more `linear >= 0` relations. Empty for a
    * guard that is not a comparison of two integer expressions, which then simply takes no part in
    * minimization: a user's own assertion may be anything at all, and a generated multi-part
    * requirement (see [[and]]) is a conjunction rather than a relation.
    *
    * Every comparison normalizes onto the same shape, so a user's `W <= 8` is comparable with a
    * generated `16 >= W` without either being rewritten. A strict comparison is the non-strict one
    * over integers, one tighter; an equality is the two directions at once, which is what lets a
    * user's `W == 8` cover a generated `W >= 8`.
    */
  private def requirementsOf(guard: ir.DFVal)(using ir.MemberGetSet): List[Requirement] =
    def diff(a: ir.DFVal, b: ir.DFVal): Requirement = ir.IntExprCalc.linearDiff(a, b)
    def tighter(req: Requirement): Requirement = req.copy(offset = req.offset - 1)
    guard match
      case ir.DFVal.Func(op = op, args = List(lhs, rhs)) =>
        op match
          case FuncOp.>=  => List(diff(lhs.get, rhs.get))
          case FuncOp.>   => List(tighter(diff(lhs.get, rhs.get)))
          case FuncOp.<=  => List(diff(rhs.get, lhs.get))
          case FuncOp.<   => List(tighter(diff(rhs.get, lhs.get)))
          case FuncOp.=== => List(diff(lhs.get, rhs.get), diff(rhs.get, lhs.get))
          case _          => Nil
      case _ => Nil

  /** Whether `stronger` leaves `weaker` with nothing to say: every relation `weaker` states is
    * already implied by one of `stronger`'s. Two relations compare only when their symbolic terms
    * cancel, and then a non-negative difference means satisfying the one satisfies the other.
    */
  private def implies(stronger: List[Requirement], weaker: List[Requirement])(using
      ir.MemberGetSet
  ): Boolean =
    weaker.nonEmpty && weaker.forall(w =>
      stronger.exists(s => ir.IntExprCalc.constOffsetDiff(w, s).exists(_ >= 0))
    )

  /** The design's own contract, as the body stated it: the requirements of its static assertions
    * whose severity makes them requirements at all. `Info` and `Warning` report, they do not
    * constrain.
    *
    * These are read as facts and never touched. The user wrote them, so they stay exactly as
    * written, in their own position, with their own message and severity; what they can do is make
    * a GENERATED constraint redundant, and having written `assert(W <= 8, ...)` the user should not
    * then be shown a generated `16 >= W` next to it.
    */
  private def userRequirements(ctx: DesignContext)(using dfc: DFC): List[List[Requirement]] =
    import dfc.getSet
    import dfhdl.compiler.analysis.isStaticAssert
    ctx.getImmutableMemberList.view.collect {
      case textOut: ir.TextOut if textOut.isStaticAssert =>
        textOut.op match
          case ir.TextOut.Op.Assert(assertionRef, Severity.Error | Severity.Fatal) =>
            requirementsOf(assertionRef.get)
          case _ => Nil
    }.filter(_.nonEmpty).toList

  /** The condition as the design states it, which is the whole of what a violation has to report.
    */
  private def report(guard: ir.DFVal)(using dfc: DFC): String =
    import dfc.getSet
    import dfhdl.compiler.printing.{Printer, DefaultPrinter}
    given printer: Printer = DefaultPrinter
    val callOwner: ir.DFOwner | ir.DFMember.Empty = dfc.ownerOption match
      case Some(owner) => owner.asIR
      case None        => ir.DFMember.Empty
    s"Design parameter violation found. Expected: ${printer.csDFValRef(guard, callOwner)}"

  /** Plants the design's pending constraints as static assertions at the tail of its body.
    *
    * Run at the end of the body, under the body's own context: the guards were built wherever their
    * checks fired, which for a check inside a conditional block is a scope the body cannot read
    * from, so each survivor's cone is CLONED here and the assertion is made over the clone. The
    * original is then read by nothing and the end-of-design sweep collects it, which is also what
    * makes a constraint dropped below cost nothing.
    *
    * MINIMIZED first, against each other and against the body's own assertions: a constraint that
    * another statement already implies says nothing, and several operations assuming related
    * relations is the normal case rather than the exception. Deduplication falls out of this, as
    * the case where two constraints imply each other.
    *
    * The tag is consumed here. It marks a PENDING constraint, and a materialized one is not
    * pending, so the clone is planted without it and no member of the finished design carries one.
    *
    * The body having run is also what makes the design's specialization to the parameters it read
    * knowable, so those equalities are raised here ([[raiseDataImpureParams]]) and take part in the
    * same minimization: an equality is the strongest statement there is about a parameter, and any
    * width relation over one it fixes is already implied by it.
    */
  private[core] def materialize()(using dfc: DFC): Unit =
    import dfc.getSet
    val ctx = dfc.mutableDB.DesignContext.current
    if (!dfc.inMetaProgramming)
      raiseDataImpureParams(ctx)
      val pending = ctx.autoConstraintGuards.map(_.setTags(_.removeTagOf[ir.AutoConstraint]))
      val kept = mutable.ListBuffer.empty[(ir.DFVal, List[Requirement])]
      if (pending.nonEmpty)
        val userReqs = userRequirements(ctx)
        pending.foreach { guard =>
          val reqs = requirementsOf(guard)
          val alreadyStated =
            if (reqs.isEmpty)
              // a guard with no comparable form takes no part: kept unless structurally repeated
              kept.exists((keptGuard, _) => keptGuard =~ guard)
            else
              userReqs.exists(implies(_, reqs)) || kept.exists((_, keptReqs) =>
                implies(keptReqs, reqs)
              )
          if (!alreadyStated)
            // this one may in turn be the stronger statement of something already kept
            if (reqs.nonEmpty) kept.filterInPlace((_, keptReqs) => !implies(reqs, keptReqs))
            kept += ((guard, reqs))
        }
      end if
      val survivors = kept.map(_._1).toList
      survivors.zipWithIndex.foreach { (guard, idx) =>
        val constraintDFC =
          dfc.emptyTags.setMeta(guard.meta).setName(constraintName(idx, survivors.length))
        val cloned = guard.cloneAnonValueAndDepsHere(using constraintDFC.anonymize)
        TextOut(
          TextOut.Op.Assert(cloned.asValOf[DFBool], Severity.Fatal)(using constraintDFC),
          List(report(cloned)(using constraintDFC)),
          Nil
        )(using constraintDFC)
      }
    end if
  end materialize
end AutoConstraint
