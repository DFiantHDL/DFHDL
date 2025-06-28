package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques, BreakOpsNoAssignments, DropUnreferencedAnons, NamedAnonMultiref,
      NamedVerilogSelection, NamedVHDLSelection, ToED, MatchToIf, SimplifyMatchSel, DropDomains,
      DropMagnets, VHDLProcToVerilog, ExplicitNamedVars, DropLocalDcls, DropOutportRead,
      GlobalizePortVectorParams, DropBAssignFromSeqProc, DropProcessAll, SimpleOrderMembers,
      ViaConnection
    )
