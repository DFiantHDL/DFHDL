package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound
import compiler.printer.Printer

trait DFBlock extends DFMember {
  ///////////////////////////////////////////////////////////////////
  // Ownership
  ///////////////////////////////////////////////////////////////////
  val isTop : Boolean = false
  ///////////////////////////////////////////////////////////////////
  def headerCodeString(implicit getset : MemberGetSet, printConfig : Printer.Config) : String
  final def codeString(body : String)(implicit getset : MemberGetSet, printConfig : Printer.Config) : String = {
    //if the body is a single row then no need for delimiters and extra new lines
    //otherwise, we add delimitation and new lines
    import printConfig.formatter._
    val delimitedBody = if (!body.contains("\n")) body else s"\n${body.delim}\n"
    s"$headerCodeString {$delimitedBody}"
  }
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  final class Context(val meta : Meta, val ownerInjector : DFMember.OwnerInjector, val db : DFDesign.DB.Mutable) extends DFMember.Context {
    lazy val owner : DFBlock = ownerInjector.get
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: DFDesign](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.ownerInjector, ctx.db)
    implicit def evTop(implicit meta: Meta, topLevel : TopLevel, lp : shapeless.LowPriority) : Context =
      new Context(meta, null, new DFDesign.DB.Mutable)
  }

  type Ref = DFMember.Ref.Of[Ref.Type, DFBlock]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
  }
}



