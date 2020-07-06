package DFiant
import DFiant.internals._

import scala.annotation.implicitNotFound
import compiler.printer.Printer

trait DFBlock extends DFOwner {
  ///////////////////////////////////////////////////////////////////
  // Ownership
  ///////////////////////////////////////////////////////////////////
  val isTop : Boolean = false
  ///////////////////////////////////////////////////////////////////
  def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String
  final def codeString(body : String)(implicit getSet : MemberGetSet, printConfig : Printer.Config) : String = {
    //if the body is a single row then no need for delimiters and extra new lines
    //otherwise, we add delimitation and new lines
    import printConfig.formatter._
    val delimitedBody =
      if (!body.contains("\n")) body.removeAlignment //single line body ==> we remove alignment for better view
      else s"\n${body.delim()}\n"
    s"$headerCodeString {$delimitedBody}"
  }
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  class Context(val meta : Meta, val container : DFOwner.Container, val dir : DFDir, val db : DFDesign.DB.Mutable, val args : ClassArgs[_])
    extends DFMember.Context
  trait VeryLowPriority {
    implicit def evCtxDefs[T <: String with Singleton](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T], meta: Meta) : Context =
      new Context(meta, ctx.container, ctx.dir, ctx.db, ctx.args)
  }
  trait LowPriority extends VeryLowPriority {
    implicit def evCtx[T <: DFDesign](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.container, ctx.dir, ctx.db, ctx.args)
  }
  object Context extends LowPriority {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evBlockContext(
      implicit meta : Meta, container : DFOwner.Container, dir : DFDir, db : DFDesign.DB.Mutable
    ) : DFBlock.Context = new DFBlock.Context(meta, container, dir, db, ClassArgs.empty)
//TODO: maybe bring back top-level DFBlock.Context
//    implicit def evTop(implicit meta: Meta, topLevel : TopLevel, lp : shapeless.LowPriority) : Context =
//      new Context(meta, null, ASIS, new DFDesign.DB.Mutable, ClassArgs.empty)
  }

  type Ref = DFMember.Ref.Of[Ref.Type, DFBlock]
  object Ref {
    trait Type extends DFOwner.Ref.Type
    implicit val ev : Type = new Type {}
  }
}



