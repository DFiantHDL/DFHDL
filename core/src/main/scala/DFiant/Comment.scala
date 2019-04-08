package DFiant

import internals._

case class Comment(comment : String)(implicit ctx0 : DFDesign.Context) extends DSLMemberConstruct {
  final private[DFiant] lazy val ctx = ctx0
  protected[DFiant] trait __DevComment extends __DevDSLMemberConstruct {
    def codeString : String = s"\n//$comment"

  }
  override private[DFiant] lazy val __dev : __DevComment = new __DevComment {}
  import __dev._
  keep
}
