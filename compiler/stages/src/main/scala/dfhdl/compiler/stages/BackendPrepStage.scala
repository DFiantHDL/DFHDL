package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUserOpaques,
      BreakOpsNoAssignments,
      DropUnreferencedAnons,
      NamedAnonMultiref,
      ExplicitRomVar,
      NamedVerilogSelection,
      NamedVHDLSelection,
      ToED,
      ApplyInvertConstraint,
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
