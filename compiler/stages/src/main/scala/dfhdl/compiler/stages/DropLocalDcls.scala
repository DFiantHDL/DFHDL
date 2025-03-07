package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage moves the local vars or named constants (at the conditional/process block level) to
  * either the design level (in Verilog) or its owner level (in VHDL), where the owner can be a
  * design or a process. Verilog does not support declarations inside an always block, so they must
  * be moved to the design level. VHDL does support declarations at the process level.
  */
case object DropLocalDcls extends Stage:
  override def dependencies: List[Stage] = List(ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val keepProcessDcls = co.backend.isVHDL
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
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
    designDB.patch(patchList)
  end transform
end DropLocalDcls

extension [T: HasDB](t: T)
  def dropLocalDcls(using co: CompilerOptions): DB =
    StageRunner.run(DropLocalDcls)(t.db)
