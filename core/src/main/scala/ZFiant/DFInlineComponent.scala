package ZFiant

import compiler.printer.Printer
abstract class DFInlineComponent[Type <: DFAny.Type](val dfType : Type)(
  implicit ctx : ContextOf[DFInlineComponent[Type]]
) extends DFDesign with DFAny.DefaultRet[Type] {
  val rep : DFInlineComponent.Rep
  override private[ZFiant] lazy val inlinedRep : Option[DFInlineComponent.Rep] = Some(rep)
  final val outPort = DFAny.Port.Out(dfType)
  final def thisVal(implicit getSet: MemberGetSet): DFAny.Of[Type] = outPort
  protected def atOwnerDo(block : => Unit) : Unit = ownerInjector.injectOwnerAndRun(ctx.owner)(block)
}
object DFInlineComponent {
  trait Rep extends Product with Serializable {
    def inlineCodeString(implicit ctx : Printer.Context) : String
  }
  type Ref = DFMember.Ref.Of[Ref.Type, DFAny]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
  }

  object Block {
    def unapply(arg : DFDesign.Block) : Option[Rep] = arg match {
      case DFDesign.Block.Internal(_,_,_,someRep) => someRep
      case _ => None
    }
  }
}

final case class Rising(bit : DFBit)(
  implicit ctx : ContextOf[Rising]
) extends DFInlineComponent[DFBool.Type](DFBool.Type(logical = true)) {
  lazy val rep : DFInlineComponent.Rep = Rising.Rep(bit)
  private val boolIn = DFBit() <> IN
  outPort <> (boolIn && !boolIn.prev())
  atOwnerDo {
    boolIn.connectWith(bit)
  }
}
object Rising {
  final case class Rep(bitRef : DFInlineComponent.Ref) extends DFInlineComponent.Rep {
    def inlineCodeString(implicit ctx : Printer.Context) : String =
      s"${bitRef.refCodeString}.rising()"
  }
  object Rep {
    def apply(bit : DFBit)(implicit ctx : DFNet.Context) : Rep = new Rep(bit)
  }
}