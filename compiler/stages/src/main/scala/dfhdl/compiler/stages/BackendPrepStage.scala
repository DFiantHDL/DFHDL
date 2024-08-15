package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques, BreakOpsNoAssignments, DropUnreferencedAnons, NamedAnonMultiref,
      NamedVerilogSelection, ToED, DropDomains, DropMagnets, VHDLProcToVerilog, ExplicitNamedVars,
      DropLocalDcls, DropBAssignFromSeqProc, SimpleOrderMembers, ViaConnection
    )
