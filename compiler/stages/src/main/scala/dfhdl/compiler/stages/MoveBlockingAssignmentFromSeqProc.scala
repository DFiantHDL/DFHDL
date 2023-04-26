package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.core.DFC.Domain
case object MoveBlockingAssignmentFromSeqProc extends Stage:
  override def dependencies: List[Stage] = List(DropLocalDcls, ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        .flatMap:
          // only sequential process blocks
          case proc: ProcessBlock if proc.isSequential =>
            val members = proc.members(MemberView.Flattened)
            var dropNets = List.empty[DFNet]
            val dsn = new MetaDesign(Domain.ED):
              members.foreach:
                // only modify nets that are blocking assignments and the assigned variable is assigned once
                case net @ DFNet.BAssignment(toVal: DFVal.Dcl, fromVal: DFVal)
                    if toVal.getAssignmentsTo.size == 1 =>
                  toVal.asVarAny <> fromVal.asValAny
                  dropNets = net :: dropNets
                case _ => // do nothing
            // patch to copy the assignments to be before the process as DFHDL connection
            (proc -> Patch.Add(
              dsn,
              Patch.Add.Config.Before
            )) ::
              // and patch to remove the old blocking assignments from the process
              dropNets.map(_ -> Patch.Remove)
          case _ => None
        .toList
    designDB.patch(patchList)
  end transform
end MoveBlockingAssignmentFromSeqProc

//This stage moves a local blocking assignment in a sequential process to outside of the process
//if that variable being assigned to is not an alias and is assigned only once.
extension [T: HasDB](t: T)
  def moveBlockingAssignmentFromSeqProc: DB =
    StageRunner.run(MoveBlockingAssignmentFromSeqProc)(t.db)
