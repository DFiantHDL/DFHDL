package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

trait DFBlock extends DFMember {
  ///////////////////////////////////////////////////////////////////
  // Ownership
  ///////////////////////////////////////////////////////////////////
  val isTop : Boolean = false
  ///////////////////////////////////////////////////////////////////
  def headerCodeString(implicit getset : MemberGetSet) : String
  final def codeString(body : String)(implicit getset : MemberGetSet) : String = {
    //if the body is a single row then no need for delimiters and extra new lines
    //otherwise, we add delimitation and new lines
    val delimitedBody = if (!body.contains("\n")) body else s"\n${body.delimRowsBy(DFCompiler.delim)}\n"
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

  class Ref extends DFMember.Ref[DFBlock]
  object Ref {
    implicit def refOf(member : DFBlock)(implicit ctx : DFMember.Context) : Ref = DFMember.Ref.newRefFor(new Ref, member)
  }

}



