package DFiant

import internals._

case class Comment(comment : String)(implicit ctx : DFDesign.Context) extends DSLMemberConstruct {
  final implicit val owner : DFAnyOwner = ctx.owner
  def codeString : String = s"\n//$comment"
  final val id = getID
  keep
}
