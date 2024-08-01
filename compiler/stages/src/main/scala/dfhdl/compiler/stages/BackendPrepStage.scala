package dfhdl.compiler.stages

case object BackendPrepStage
    extends BundleStage(
      DropUnreferencedAnons, NamedAnonMultiref, DropUserOpaques, NamedVerilogSelection, ToED,
      DropDomains, DropMagnets, VHDLProcToVerilog, ExplicitNamedVars, DropLocalDcls,
      DropBAssignFromSeqProc, SimpleOrderMembers, ViaConnection
    )
