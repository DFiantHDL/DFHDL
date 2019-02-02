package DFiant

import internals._

case class Comment(comment : String)(implicit ctx : DFDesign.Context) extends DSLMemberConstruct {
  import __dev._
  final override val ownerOption : Option[DFAnyOwner] = ctx.ownerOption
  def codeString : String = s"\n//$comment"
  private[DFiant] lazy val nameIt = ctx.n
  final val id = getID
  keep
}
