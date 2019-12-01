package ZFiant
import DFiant.internals.Meta

trait DFBlock extends DFMember {self =>
  protected implicit def __anyContext(implicit meta0 : Meta) : DFAny.Context = new DFAny.Context {
    val meta: Meta = meta0
    val owner: DFBlock = self
  }
  private[ZFiant] var __injectedOwner : DFBlock = self
  protected implicit def __blockContext(implicit meta0 : Meta) : DFBlock.Context = new DFBlock.Context {
    val meta: Meta = meta0
    val ownerOption : Option[DFBlock] = Some(__injectedOwner)
  }
}

object DFBlock {
  trait Context extends DFMember.Context {
    val meta : Meta
    val ownerOption : Option[DFBlock]
    final lazy val owner : DFBlock = ownerOption.get
  }

}

sealed trait ConditionalBlock[CB <: ConditionalBlock[CB, Ret], Ret] extends DFBlock {
  val block : () => Ret
  private val originalOwner : DFBlock = owner.__injectedOwner
  owner.__injectedOwner = this
  final val returnValue : Ret = block()
  owner.__injectedOwner = originalOwner
}

object ConditionalBlock {
  sealed trait WithRetVal[CB <: WithRetVal[CB, Type], Type <: DFType] extends ConditionalBlock[WithRetVal[CB, Type], DFAny.Val[Type, _]] with DFAny.Val[Type, false]
  object WithRetVal {
    final case class IfBlock[Type <: DFType](dfType : Type, cond : DFAny.Val[DFBool, _], block : () => DFAny.Val[Type, _])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf(block : => DFAny.Val[Type, _])(implicit ctx : DFBlock.Context) : ElseBlock[Type] = ElseBlock[Type](dfType, () => block, Left(this))(ctx)
      def elseifdf(cond : DFAny.Val[DFBool, _])(block : => DFAny.Val[Type, _])(implicit ctx : DFBlock.Context) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, cond, () => block, Left(this))(ctx)
    }
    final case class ElseIfBlock[Type <: DFType](dfType : Type, cond : DFAny.Val[DFBool, _], block : () => DFAny.Val[Type, _], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf(block : => DFAny.Val[Type, _])(implicit ctx : DFBlock.Context) : ElseBlock[Type] = ElseBlock[Type](dfType, () => block, Right(this))(ctx)
      def elseifdf(cond : DFAny.Val[DFBool, _])(block : => DFAny.Val[Type, _])(implicit ctx : DFBlock.Context) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, cond, () => block, Right(this))(ctx)
    }
    final case class ElseBlock[Type <: DFType](dfType : Type, block : () => DFAny.Val[Type, _], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type]
  }
  sealed trait NoRetVal[CB <: NoRetVal[CB]] extends ConditionalBlock[NoRetVal[CB], Unit]
  object NoRetVal {}
}

//trait DFDesign {
//
//}

//case class DFDesign(members : List[DFAnyMember]) extends DFAnyOwner
