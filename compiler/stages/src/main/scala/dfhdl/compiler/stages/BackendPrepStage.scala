package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques, BreakOpsNoAssignments, DropUnreferencedAnons, NamedAnonMultiref,
      NamedVerilogSelection, ToED, MatchToIf, DropDomains, DropMagnets, VHDLProcToVerilog,
      ExplicitNamedVars, DropLocalDcls, DropBAssignFromSeqProc, DropProcessAll, SimpleOrderMembers,
      ViaConnection
    )
