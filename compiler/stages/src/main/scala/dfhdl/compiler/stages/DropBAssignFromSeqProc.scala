package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
case object DropBAssignFromSeqProc extends Stage:
  override def dependencies: List[Stage] = List(DropLocalDcls, ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] =
      var latestSeqProc: Option[ProcessBlock] = None
      designDB.members.view
        .flatMap:
          case proc: ProcessBlock =>
            // cache the latest sequential process we passed through
            if (proc.isSequential) latestSeqProc = Some(proc)
            else latestSeqProc = None
            None
          // only modify nets that are blocking assignments and the assigned variable is assigned once
          case net @ DFNet.BAssignment(toVal: DFVal.Dcl, fromVal: DFVal)
              if latestSeqProc.map(proc => net.isInsideOwner(proc)).getOrElse(false) &&
                toVal.getAssignmentsTo.size == 1 =>
            // blocking assignment changed to connection (to be moved outside a process)
            val connNet = net.copy(op = DFNet.Op.Connection)
            val changeToConnPatch =
              net -> Patch.Replace(connNet, Patch.Replace.Config.ChangeRefAndRemove)
            val movedMembers = net.collectRelMembers :+ connNet
            // relevant members must move alongside the net
            List(
              changeToConnPatch,
              // patch to move the assignments to be before the process as DFHDL connection
              latestSeqProc.get -> Patch.Move(
                movedMembers,
                net.getOwnerBlock,
                Patch.Move.Config.Before
              )
            )
          case _ => None
        .toList
    end patchList
    designDB.patch(patchList)
  end transform
end DropBAssignFromSeqProc

//This stage moves a local blocking assignment in a sequential process to outside of the process
//if that variable being assigned to is not an alias and is assigned only once.
extension [T: HasDB](t: T)
  def dropBAssignFromSeqProc(using CompilerOptions): DB =
    StageRunner.run(DropBAssignFromSeqProc)(t.db)
