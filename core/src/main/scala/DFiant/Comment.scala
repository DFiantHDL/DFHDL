package DFiant

import internals._

case class Comment(comment : String)(implicit ctx : DFDesign.Context) extends DSLMemberConstruct {
  protected[DFiant] trait __Dev extends super[DSLMemberConstruct].__Dev {
    def codeString : String = s"\n//$comment"

  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  import __dev._
  final override val ownerOption : Option[DFAnyOwner] = ctx.ownerOption
  private[DFiant] lazy val nameIt = ctx.n
  keep
}
