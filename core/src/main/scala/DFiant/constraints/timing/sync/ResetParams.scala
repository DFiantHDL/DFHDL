package DFiant
package constraints.timing.sync

final case class ResetParams(
  name : String, mode : ResetParams.Mode, active : ResetParams.Active
) extends DFMember.CustomTagOf[DFDesign.Block] {
  val activeInt : Int = active match {
    case ResetParams.Active.Low => 0
    case ResetParams.Active.High => 1
  }
  val inactiveInt : Int = 1 - activeInt
}
object ResetParams {
  sealed trait Mode extends Product with Serializable
  object Mode {
    case object Async extends Mode
    case object Sync extends Mode
  }
  sealed trait Active extends Product with Serializable
  object Active {
    case object Low extends Active
    case object High extends Active
  }
  final val default = ResetParams("rst", Mode.Async, Active.Low)
  def get(implicit getSet: MemberGetSet) : ResetParams =
    getSet.designDB.top.getTagOf[ResetParams].getOrElse(default)
}

