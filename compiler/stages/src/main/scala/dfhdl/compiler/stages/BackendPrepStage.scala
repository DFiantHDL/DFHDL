package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUnreferencedAnons, NamedAnonMultiref, DropUserOpaques, ToED, DropDomains, DropMagnets,
      VHDLProcToVerilog, NamedVerilogSelection, ExplicitNamedVars, DropLocalDcls,
      DropBAssignFromSeqProc, SimpleOrderMembers, ViaConnection
    )
