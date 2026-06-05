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
  * declarations, so only conditional-block-level declarations need lifting in VHDL.
  *
  * ==Rules==
  *
  * ===Rule 1: Declarations inside conditional blocks===
  * Any local variable (`VAR`) or non-global named constant (`CONST`) that is declared inside a
  * conditional block (`if`, `match`, or `while`) is moved to before the top-level conditional
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
  * ===Rule 3: Declarations inside step blocks===
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
  */
//format: on
case object DropLocalDcls extends HierarchyStage:
  override def dependencies: List[Stage] = List(ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  def transformSubDB(rootDB: DB)(using getSet: MemberGetSet, co: CompilerOptions, rg: RefGen): DB =
    val keepProcessDcls = co.backend.isVHDL
    val patches = subDB.members.view
      // only var or constant declarations,
      // and we also require their anonymous dependencies
      .flatMap {
        // skip iterator declarations
        case IteratorDcl()                 => None
        case m @ DclVar()                  => m.collectRelMembers(includeOrigVal = true)
        case m @ DclConst() if !m.isGlobal => m.collectRelMembers(includeOrigVal = true)
        case _                             => None
      }
      .flatMap(dclMovePatch(_, keepProcessDcls))
      .toList
    subDB.patch(patches)
  end transformSubDB

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
      // when the declaration actually needs to escape an enclosing conditional or step block.
      case _ =>
        if anchor ne dcl then Some(anchor -> Patch.Move(dcl, Patch.Move.Config.Before))
        else None

  // Climbs from a member up through enclosing conditional and step blocks, returning the outermost
  // in-scope anchor to move before, paired with the nearest enclosing non-conditional, non-step
  // scope block (a ProcessBlock or DFDesignBlock, or a loop block). When the member is inside a
  // conditional, the anchor is the top-level conditional header for that scope; otherwise it is the
  // member itself. Step blocks are escaped one level at a time until a non-step scope is reached.
  @tailrec private def climbToScope(m: DFMember)(using MemberGetSet): (DFMember, DFBlock) =
    val (anchor, scopeBlock) = m.getOwnerBlock match
      case cb: DFConditional.Block =>
        val topCondHeader = cb.getTopConditionalHeader
        (topCondHeader, topCondHeader.getOwnerBlock)
      case b => (m, b)
    scopeBlock match
      case sb: StepBlock => climbToScope(sb)
      case _             => (anchor, scopeBlock)
end DropLocalDcls

extension [T: HasDB](t: T)
  def dropLocalDcls(using co: CompilerOptions): DB =
    StageRunner.run(DropLocalDcls)(t.db)
