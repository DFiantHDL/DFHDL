package DFiant
package constraints.timing.sync

final case class ClockParams(name : String, edge : ClockParams.Edge = ClockParams.Edge.Rising) extends DFMember.CustomTagOf[DFDesign.Block] {
  val activeInt : Int = edge match {
    case ClockParams.Edge.Rising => 1
    case ClockParams.Edge.Falling => 0
  }
  val inactiveInt : Int = 1 - activeInt

}
object ClockParams {
  type Edge = EdgeDetect.Edge
  final val Edge = EdgeDetect.Edge
  final val default = ClockParams("clk", Edge.Rising)
  def get(implicit getSet: MemberGetSet) : ClockParams = getSet.designDB.top.getTagOf[ClockParams].getOrElse(default)
}

