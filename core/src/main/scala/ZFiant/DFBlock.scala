package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

trait DFBlock extends DFMember with Implicits {self =>
  val ctx : DFBlock.Context
  protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    DFAny.Context(meta, self)
  private[ZFiant] var __injectedOwner : DFBlock = self
  protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    DFBlock.Context(meta, Some(__injectedOwner))
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  final case class Context(meta : Meta, ownerOption : Option[DFBlock]) extends DFMember.Context {
    lazy val owner : DFBlock = ownerOption.get
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evTop(implicit meta: Meta, allowTOP : TopLevel, lp : shapeless.LowPriority) : Context =
      new Context(meta, None)
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
  sealed trait WithRetVal[CB <: WithRetVal[CB, Type], Type <: DFAny.Type] extends ConditionalBlock[WithRetVal[CB, Type], DFAny.Of[Type]] with DFAny.ValOrVar[Type, false]
  object WithRetVal {
    final case class IfBlock[Type <: DFAny.Type](dfType : Type, cond : DFBool, block : () => DFAny.Of[Type])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf(block : => DFAny.Of[Type])(implicit ctx : DFBlock.Context) : ElseBlock[Type] = ElseBlock[Type](dfType, () => block, Left(this))(ctx)
      def elseifdf(cond : DFBool)(block : => DFAny.Of[Type])(implicit ctx : DFBlock.Context) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, cond, () => block, Left(this))(ctx)
    }
    final case class ElseIfBlock[Type <: DFAny.Type](dfType : Type, cond : DFBool, block : () => DFAny.Of[Type], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type] {
      def elsedf(block : => DFAny.Of[Type])(implicit ctx : DFBlock.Context) : ElseBlock[Type] = ElseBlock[Type](dfType, () => block, Right(this))(ctx)
      def elseifdf(cond : DFBool)(block : => DFAny.Of[Type])(implicit ctx : DFBlock.Context) : ElseIfBlock[Type] = ElseIfBlock[Type](dfType, cond, () => block, Right(this))(ctx)
    }
    final case class ElseBlock[Type <: DFAny.Type](dfType : Type, block : () => DFAny.Of[Type], prevBlock : Either[IfBlock[Type], ElseIfBlock[Type]])(implicit val ctx : DFBlock.Context) extends WithRetVal[IfBlock[Type], Type]
  }
  sealed trait NoRetVal[CB <: NoRetVal[CB]] extends ConditionalBlock[NoRetVal[CB], Unit]
  object NoRetVal {}
}
