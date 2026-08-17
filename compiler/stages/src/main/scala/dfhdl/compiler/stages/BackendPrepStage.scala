package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropPhysicalValues,
      DropUserOpaques,
      BreakOpsNoAssignments,
      DropUnreferencedAnons,
      NamedAnonMultiref,
      ExplicitRomVar,
      NamedVerilogSelection,
      NamedVHDLSelection,
      ToED,
      DropForkJoinsED,
      DropLocalBlocksED,
      ApplyInvertConstraint,
      DropWholeVecAssign,
      DropStructsVecs,
      MatchToIf,
      SimplifyMatchSel,
      DropDomains,
      DropMagnets,
      ConnectUnused,
      VHDLProcToVerilog,
      VerilogProcToVHDL,
      ExplicitNamedVars,
      ExplicitCondExprAssign,
      DropLocalDcls,
      DropOutportRead,
      GlobalizePortVectorParams,
      DropBAssignFromSeqProc,
      DropProcessAll,
      SimpleOrderMembers,
      LocalToDesignParams,
      DropDesignParamDeps,
      ViaConnection,
      // LAST: it flattens the namespace-derived packages into names for a backend that has no
      // packages, so it should see only what actually survives to the emission (v95/v2001 drop
      // structs and opaques on the way here), and the names it produces must reach
      // `<backend>UniqueNames`, which runs after this bundle
      DropPackages
    )
