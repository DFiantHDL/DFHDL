package ZFiant.maxeler
import ZFiant._

case class MaxJNode(
  design : DFDesign,
  streamInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  streamOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]],
  scalarInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  scalarOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]]
) {
  private val designDB = design.db
  private val streamInExtras = (streamInputs, streamInputs.map(p => new DFDesign() {
    final val empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val streamOutExtras = (streamOutputs, streamOutputs.map(p => new DFDesign() {
    final val stall = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val scalarInExtras = (scalarInputs, scalarInputs.map(p => new DFDesign() {
    final val reg = p.prev() setNamePrefix(s"${p.name}_")
  })).zipped

  val db : DFDesign.DB = {
    import designDB.getset
    import DFDesign.DB.Patch
    designDB
      .patch(streamInExtras.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
      .patch(streamInExtras.map((p, _) => p -> Patch.Replace(p.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
      .patch(streamOutExtras.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
      .patch(streamOutExtras.map((p, _) => p -> Patch.Replace(p.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
      .patch(scalarInExtras.map((p, e) => p -> Patch.Replace(e.reg, Patch.Replace.Config.ChangeRefOnly)))
      .patch(scalarInExtras.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
  }
}
