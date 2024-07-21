package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUnreferencedAnons, NamedAnonMultiref, ToED, DropDomains, DropMagnets, VHDLProcToVerilog,
      NamedVerilogSelection, ExplicitNamedVars, DropLocalDcls, DropBAssignFromSeqProc,
      SimpleOrderMembers, ViaConnection
    )
