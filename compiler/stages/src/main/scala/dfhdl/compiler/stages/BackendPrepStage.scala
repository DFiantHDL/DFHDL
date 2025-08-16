package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques,
      BreakOpsNoAssignments,
      DropUnreferencedAnons,
      NamedAnonMultiref,
      ApplyInvertConstraint,
      ExplicitRomVar,
      NamedVerilogSelection,
      NamedVHDLSelection,
      ToED,
      DropStructsVecs,
      MatchToIf,
      SimplifyMatchSel,
      DropDomains,
      DropMagnets,
      VHDLProcToVerilog,
      ExplicitNamedVars,
      DropLocalDcls,
      DropOutportRead,
      GlobalizePortVectorParams,
      LocalToDesignParams,
      DropDesignParamDeps,
      DropBAssignFromSeqProc,
      DropProcessAll,
      SimpleOrderMembers,
      ViaConnection
    )
