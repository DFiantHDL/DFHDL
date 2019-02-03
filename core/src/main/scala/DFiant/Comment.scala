package DFiant

import internals._

case class Comment(comment : String)(implicit ctx : DFDesign.Context) extends DSLMemberConstruct {
  import __dev._
  protected[DFiant] trait __Dev extends super[DSLMemberConstruct].__Dev {
    final val id = getID
    def codeString : String = s"\n//$comment"

  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  final override val ownerOption : Option[DFAnyOwner] = ctx.ownerOption
  private[DFiant] lazy val nameIt = ctx.n
  keep
}
