package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques, BreakOpsNoAssignments, DropUnreferencedAnons,
      NamedAnonMultiref, ExplicitRomVar, DropStructsVecs, NamedVerilogSelection, NamedVHDLSelection,
      ToED, MatchToIf, SimplifyMatchSel, DropDomains, DropMagnets, VHDLProcToVerilog,
      ExplicitNamedVars, DropLocalDcls, DropOutportRead, GlobalizePortVectorParams,
      LocalToDesignParams, DropDesignParamDeps, DropBAssignFromSeqProc, DropProcessAll,
      SimpleOrderMembers, ViaConnection
    )
