package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec

//format: off
/** This stage moves local variable and constant declarations out of their lexical scope to a
  * position where the target language supports them. It also inserts reset-to-init assignments for
  * local variables with initialization values inside RT process blocks.
  *
  * ==Context==
  *
  * Verilog `always` blocks (process blocks) do not support variable declarations — all declarations
  * must appear at the design (module) level. VHDL `process` blocks DO support variable
  * declarations, so under VHDL only declarations nested inside a conditional, loop or step
  * block need lifting.
  *
  * ==REG exception==
  *
  * A register declaration (`VAR.REG`) holds state across clock cycles and must be emitted as a
  * design-level signal rather than a process variable. Therefore, under VHDL, a `REG` declaration is
  * always moved out to the design level (before the process block), exactly as under Verilog — the
  * VHDL "keep inside the process" rules below apply only to non-`REG` local variables and constants.
  *
  * ==Rules==
  *
  * ===Rule 1: Declarations inside conditional blocks===
  * Any local variable (`VAR`) or non-global named constant (`CONST`) that is declared inside a
  * conditional block (`if` or `match`) is moved to before the top-level conditional
  * header that contains it:
  *   - For non-VHDL backends: if the top-level conditional is itself inside a process block, the
  *     declaration is moved to before the process block (design level).
  *   - For VHDL: the declaration is moved to just before the top-level conditional header, staying
  *     inside the process block if one exists.
  * {{{
  * // Before — zz declared inside an if inside a process
  * class ID extends EDDesign:
  *   process(all):
  *     if (x > 5)
  *       val zz = SInt(16) <> VAR
  *       ...
  *
  * // After (Verilog) — moved to design level, before the process
  * class ID extends EDDesign:
  *   val zz = SInt(16) <> VAR
  *   process(all):
  *     if (x > 5)
  *       ...
  *
  * // After (VHDL) — moved to just before the if, inside the process
  * class ID extends EDDesign:
  *   process(all):
  *     val zz = SInt(16) <> VAR
  *     if (x > 5)
  *       ...
  * }}}
  *
  * ===Rule 2: Declarations directly inside process blocks===
  * A local variable declared directly inside a process block (not inside a conditional):
  *   - For non-VHDL backends: moved to before the process block (design level).
  *   - For VHDL: moved to the top of the process block (before any statements), because VHDL
  *     requires all variable declarations to precede statements in a process.
  * {{{
  * // Before
  * class ID extends EDDesign:
  *   process(all):
  *     stmt1
  *     val zz = SInt(16) <> VAR
  *     stmt2
  *
  * // After (Verilog) — moved to design level
  * class ID extends EDDesign:
  *   val zz = SInt(16) <> VAR
  *   process(all):
  *     stmt1
  *     stmt2
  *
  * // After (VHDL) — moved to top of process
  * class ID extends EDDesign:
  *   process(all):
  *     val zz = SInt(16) <> VAR
  *     stmt1
  *     stmt2
  * }}}
  *
  * ===Rule 3: Declarations inside loop blocks===
  * Neither target language can hold a declaration in a loop body: VHDL forbids it outright, and
  * the Verilog printer emits one without its terminating `;`. A local variable or constant
  * declared inside a `for` or `while` block is therefore lifted out of the loop, to the same place
  * Rule 1 and Rule 2 would put it, which is unchanged in meaning because a loop body is one IR
  * scope rather than one per iteration.
  * {{{
  * // Before — zz declared inside a for loop inside a process
  * class ID extends EDDesign:
  *   process(all):
  *     for (i <- 0 until 4)
  *       val zz = SInt(16) <> VAR
  *       ...
  *
  * // After (Verilog) — moved to design level, before the process
  * class ID extends EDDesign:
  *   val zz = SInt(16) <> VAR
  *   process(all):
  *     for (i <- 0 until 4)
  *       ...
  *
  * // After (VHDL) — moved to just before the loop, inside the process
  * class ID extends EDDesign:
  *   process(all):
  *     val zz = SInt(16) <> VAR
  *     for (i <- 0 until 4)
  *       ...
  * }}}
  *
  * ===Rule 4: Declarations inside step blocks===
  * A local variable or constant declared inside a `StepBlock` (an RT FSM state) — either directly
  * or nested inside a conditional within the step — is lifted out of the step, because the FSM
  * states generated from steps cannot carry declarations:
  *   - For non-VHDL backends: moved to before the enclosing process block (design level).
  *   - For VHDL: moved to just before the outermost (top-level) step block, keeping the declaration
  *     at the process-body level where VHDL allows process variable declarations.
  * {{{
  * // Before — zz declared inside a step
  * class ID extends RTDesign:
  *   process:
  *     def S_0: Step =
  *       val zz = SInt(16) <> VAR
  *       ...
  *
  * // After (Verilog) — moved to design level, before the process
  * class ID extends RTDesign:
  *   val zz = SInt(16) <> VAR
  *   process:
  *     def S_0: Step =
  *       ...
  *
  * // After (VHDL) — moved to just before the top-level step, inside the process
  * class ID extends RTDesign:
  *   process:
  *     val zz = SInt(16) <> VAR
  *     def S_0: Step =
  *       ...
  * }}}
  *
  * ===Rule 5: Combinational don't-care defaults for lifted variables===
  * Lifting a variable out of a conditional scope inside a combinational process (`process(all)`)
  * extends its lifetime: the variable is then driven on only some of the process paths, so
  * synthesis infers a latch for it, which is a hard error for Verilog `always_comb`. Since the
  * variable's original scope ended with the conditional, nothing can observe it across process
  * activations, and a don't-care default assignment placed at the position the declaration
  * escaped from keeps the process combinationally complete without changing behavior:
  * {{{
  * // Before — tmp declared (without init) inside an if inside a combinational process
  * class ID extends EDDesign:
  *   process(all):
  *     res := a
  *     if (sel)
  *       val tmp = UInt(8) <> VAR
  *       tmp := a + b
  *       res := tmp
  *
  * // After (Verilog) — moved to design level, don't-care default before the if
  * class ID extends EDDesign:
  *   val tmp = UInt(8) <> VAR
  *   process(all):
  *     res := a
  *     tmp := d"8'?"
  *     if (sel)
  *       tmp := a + b
  *       res := tmp
  * }}}
  * The default applies only when all of the following hold, since otherwise the lift does not
  * create an incompletely-driven combinational variable:
  *   - the enclosing process is `process(all)` (an explicit sensitivity list keeps the target
  *     language's own semantics, and a clocked process infers registers, not latches)
  *   - the declaration has no init (an init declares deliberate state retention, which a
  *     per-activation default would break)
  *   - the escaped scope is genuinely conditional: an `if`/`match` branch or a `while` body
  *     (a `for` body iterates a static range, so its contents are driven on every activation)
  */
//format: on
case object DropLocalDcls extends HierarchyStage:
  override def dependencies: List[Stage] = List(ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  def transformSubDB(rootDB: DB)(using getSet: MemberGetSet, co: CompilerOptions, rg: RefGen): DB =
    val patches = subDB.members.view
      // only var or constant declarations,
      // and we also require their anonymous dependencies
      .flatMap {
        // skip iterator declarations
        case IteratorDcl() => None
        // A REG declaration holds state across clock cycles and must become a
        // design-level signal (it cannot be a VHDL process variable), so under
        // VHDL it is moved out of the process just like under Verilog.
        case m @ DclVar()                  => Some(m -> (co.backend.isVHDL && !m.isReg))
        case m @ DclConst() if !m.isGlobal => Some(m -> co.backend.isVHDL)
        case _                             => None
      }
      .flatMap: (dcl, keepProcessDcls) =>
        // the don't-care default (when needed) is listed AFTER the moves, so that under VHDL its
        // `Add` merges into the same-anchor `Move` with the assignment placed right after the
        // moved declaration
        dcl.collectRelMembers(includeOrigVal = true).flatMap(dclMovePatch(_, keepProcessDcls)) ++
          combDefaultPatch(dcl)
      .toList
    subDB.patch(patches)
  end transformSubDB

  // Rule 5 (see the stage doc): a variable lifted out of a conditional scope inside a
  // combinational process receives a don't-care default assignment at the position the
  // declaration escaped from, keeping the process combinationally complete (issue #438).
  private def combDefaultPatch(dclVal: DFVal)(using
      MemberGetSet,
      RefGen
  ): Option[(DFMember, Patch)] =
    dclVal match
      case dcl @ DclVar() if !dcl.isReg && dcl.initRefList.isEmpty && insideConditional(dcl) =>
        val (anchor, scopeBlock) = climbToScope(dcl)
        scopeBlock match
          case pb: ProcessBlock =>
            pb.sensitivity match
              case ProcessBlock.Sensitivity.All =>
                val dsn = new MetaDesign(anchor, Patch.Add.Config.Before):
                  dcl.asVarAny.:=(
                    dfhdl.core.Bubble.constValOf(new dfhdl.core.DFType(dcl.dfType), named = false)
                  )(using dfc.setMetaAnon(dcl.meta.position))
                Some(dsn.patch)
              case _ => None
          case _ => None
      case _ => None

  // Whether the member's lexical scope is conditioned: an enclosing block between the member and
  // its nearest scope block (process or design) decides per activation whether the member's scope
  // executes. Step and for-loop blocks are climbed through (a for body iterates a static range),
  // while a while body only executes when its guard holds.
  @tailrec private def insideConditional(m: DFMember)(using MemberGetSet): Boolean =
    m.getOwnerBlock match
      case _: DFConditional.Block => true
      case _: DFLoop.DFWhileBlock => true
      case sb: StepBlock          => insideConditional(sb)
      case lb: DFLoop.DFForBlock  => insideConditional(lb)
      case _                      => false

  // Computes the move patch (if any) relocating a local declaration `dcl` out of its lexical scope
  // to a position supported by the target language. Returns `None` when the declaration is already
  // at a valid position (directly at design level, or — under VHDL — directly inside a process).
  private def dclMovePatch(dcl: DFMember, keepProcessDcls: Boolean)(using
      MemberGetSet
  ): Option[(DFMember, Patch)] =
    val (anchor, scopeBlock) = climbToScope(dcl)
    scopeBlock match
      // non-VHDL: declarations are not allowed inside process blocks, so move to the design level
      // before the process block.
      case pb: ProcessBlock if !keepProcessDcls =>
        Some(pb -> Patch.Move(dcl, Patch.Move.Config.Before))
      // VHDL process scope, or design scope: move before the outermost in-scope anchor, but only
      // when the declaration actually needs to escape an enclosing conditional, loop or step
      // block.
      case _ =>
        if anchor ne dcl then Some(anchor -> Patch.Move(dcl, Patch.Move.Config.Before))
        else None
  end dclMovePatch

  // Climbs from a member up through enclosing conditional, step and loop blocks, returning the
  // outermost in-scope anchor to move before, paired with the nearest enclosing scope block that
  // can hold a declaration (a ProcessBlock or a DFDesignBlock). When the member is inside a
  // conditional, the anchor is the top-level conditional header for that scope; otherwise it is the
  // member itself. Step and loop blocks are escaped one level at a time until such a scope is
  // reached.
  @tailrec private def climbToScope(m: DFMember)(using MemberGetSet): (DFMember, DFBlock) =
    val (anchor, scopeBlock) = m.getOwnerBlock match
      case cb: DFConditional.Block =>
        val topCondHeader = cb.getTopConditionalHeader
        (topCondHeader, topCondHeader.getOwnerBlock)
      case b => (m, b)
    scopeBlock match
      case sb: StepBlock    => climbToScope(sb)
      case lb: DFLoop.Block => climbToScope(lb)
      case _                => (anchor, scopeBlock)
end DropLocalDcls

extension [T: HasDB](t: T)
  def dropLocalDcls(using co: CompilerOptions): DB =
    StageRunner.run(DropLocalDcls)(t.db)
