package DFiant
package constraints.timing.sync

final case class ClockParams(name : String, edge : ClockParams.Edge = ClockParams.Edge.Rising) extends DFMember.CustomTagOf[DFDesign.Block]
object ClockParams {
  type Edge = EdgeDetect.Edge
  final val Edge = EdgeDetect.Edge
  final val default = ClockParams("clk", Edge.Rising)
  def get(implicit getSet: MemberGetSet) : ClockParams = getSet.designDB.top.getTagOf[ClockParams].getOrElse(default)
}

