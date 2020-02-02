package ZFiant

abstract class DFInlineComponent[Type <: DFAny.Type](val dfType : Type)(
  implicit ctx : ContextOf[DFInlineComponent[Type]]
) extends DFDesign with DFAny.DefaultRet[Type] {
  override private[ZFiant] lazy val inlinedRep : Option[MemberGetSet => String] = Some(_ => inlineCodeString)
  final val outPort = DFAny.Port.Out(dfType)
  def inlineCodeString(implicit getset : MemberGetSet) : String
  final def thisVal(implicit getSet: MemberGetSet): DFAny.Of[Type] = outPort
  protected def atOwnerDo(block : => Unit) : Unit = ownerInjector.injectOwnerAndRun(ctx.owner)(block)
}

final case class Rising(bool : DFBool)(
  implicit ctx : ContextOf[Rising]
) extends DFInlineComponent[DFBool.Type](DFBool.Type()) {
  private val boolIn = DFBool() <> IN
  outPort <> (boolIn && !boolIn.prev())
  atOwnerDo {
    boolIn.connectWith(bool)
  }
  override def inlineCodeString(implicit getset : MemberGetSet) : String =
    s"${bool.refCodeString(ctx.owner, getset)}.rising()"
}