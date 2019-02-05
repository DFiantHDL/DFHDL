package DFiant

import internals._

case class Comment(comment : String)(implicit ctx0 : DFDesign.Context) extends DSLMemberConstruct {
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __Dev extends super[DSLMemberConstruct].__Dev {
    def codeString : String = s"\n//$comment"

  }
  override private[DFiant] lazy val __dev : TDev = new __Dev {}.asInstanceOf[TDev]
  import __dev._
  keep
}
