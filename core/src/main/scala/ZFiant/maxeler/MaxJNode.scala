package ZFiant.maxeler
import ZFiant._

case class MaxJNode(
  design : DFDesign,
  pullInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  pullOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]],
  pushInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  pushOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]],
  scalarInputs : List[DFAny.PortInOf[_ <: DFAny.Type]],
  scalarOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]]
) {
  private val designDB = design.db
  private val pullInZ = (pullInputs, pullInputs.map(p => new DFDesign() {
    final val empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val pullOutZ = (pullOutputs, pullOutputs.map(p => new DFDesign() {
    final val empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushInZ = (pushInputs, pushInputs.map(p => new DFDesign() {
    final val stall = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushOutZ = (pushOutputs, pushOutputs.map(p => new DFDesign() {
    final val stall = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val scalaInZ = (scalarInputs, scalarInputs.map(p => new DFDesign() {
    final val reg = p.prev() setNamePrefix(s"${p.name}_")
  })).zipped

  val db : DFDesign.DB = {
    import designDB.getset
    import DFDesign.DB.Patch
    import DFCompiler._
    designDB
      .patch(pullInZ.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
      .patch(pullInZ.map((p, _) => p -> Patch.Replace(p.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
      .patch(pushOutZ.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
      .patch(pushOutZ.map((p, _) => p -> Patch.Replace(p.setNameSuffix("_data"), Patch.Replace.Config.FullReplacement)))
      .patch(scalaInZ.map((p, e) => p -> Patch.Replace(e.reg, Patch.Replace.Config.ChangeRefOnly)))
      .patch(scalaInZ.map((p, e) => p -> DFDesign.DB.Patch.Add(e.db, before = false)))
//      .fixNames
      .moveConnectableFirst
  }
}
