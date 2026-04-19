package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

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
  */
//format: on
case object DropLocalDcls extends HierarchyStage:
  override def dependencies: List[Stage] = List(ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  def transformSubDB(subDB: DB)(using getSet: MemberGetSet, co: CompilerOptions, rg: RefGen): DB =
    val keepProcessDcls = co.backend.isVHDL
    val patches = subDB.members.view
        // only var or constant declarations ,
        // and we also require their anonymous dependencies
        .flatMap {
          // skip iterator declarations
          case IteratorDcl()                 => None
          case m @ DclVar()                  => m.collectRelMembers(includeOrigVal = true)
          case m @ DclConst() if !m.isGlobal => m.collectRelMembers(includeOrigVal = true)
          case _                             => None
        }
        .map(m => (m, m.getOwnerBlock))
        .flatMap {
          // declarations inside conditional blocks
          case (dcl, cb: DFConditional.Block) =>
            val topCondHeader = cb.getTopConditionalHeader
            // if we don't keep process vars, we check if the owner is a process block,
            // and if so, we need to move the declarations before it.
            val moveBeforeMember = topCondHeader.getOwnerBlock match
              case pb: ProcessBlock if !keepProcessDcls => pb
              case _                                    => topCondHeader
            Some(moveBeforeMember -> Patch.Move(dcl, Patch.Move.Config.Before))
          // declarations inside process blocks if we should not keep them
          case (dcl, pb: ProcessBlock) if !keepProcessDcls =>
            Some(pb -> Patch.Move(dcl, Patch.Move.Config.Before))
          case _ => None
        }
        .toList
    subDB.patch(patches)
  end transformSubDB
end DropLocalDcls

extension [T: HasDB](t: T)
  def dropLocalDcls(using co: CompilerOptions): DB =
    StageRunner.run(DropLocalDcls)(t.db)
