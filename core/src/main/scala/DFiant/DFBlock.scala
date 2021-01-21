package DFiant
import compiler.csprinter.CSPrinter
import DFiant.internals._

import scala.annotation.implicitNotFound
import compiler.printer.formatter._

trait DFBlock extends DFOwner {
  def headerCodeString(implicit printer: CSPrinter) : String
  final def codeString(body : String)(implicit printer: CSPrinter) : String = {
    //if the body is a single row then no need for delimiters and extra new lines
    //otherwise, we add delimitation and new lines
    val delimitedBody =
      if (!body.contains("\n")) body.removeAlignment //single line body ==> we remove alignment for better view
      else s"\n${body.delim()}\n"
    s"$headerCodeString {$delimitedBody}"
  }
}

object DFBlock {
  @implicitNotFound(Context.MissingError.msg)
  class Context(val meta : Meta, val symbol : Meta.SymbolOf[_], val container : DFOwner.Container, val dir : DFDir, val db : DFDesign.DB.Mutable, val args : ClassArgs[_])
    extends DFMember.Context {
    def setName(name : String) = new Context(meta.setName(name), symbol, container, dir, db, args)
    def atOwnerDo(block : => Unit) : Unit =
      db.OwnershipContext.injectOwnerAndRun(container, owner.getOwner(db.getSet))(block)
  }
  trait VeryLowPriority {
    implicit def evCtxDefs[T <: String with Singleton](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T], meta: Meta) : Context =
      new Context(ctx.meta, ctx.symbol, ctx.container, ctx.dir, ctx.db, ctx.args)
  }
  trait LowPriority extends VeryLowPriority {
    implicit def evCtx[T <: DFDesign](implicit ctx : ContextOf[T], mustBeTheClassOf: MustBeTheClassOf[T]) : Context =
      new Context(ctx.meta, ctx.symbol, ctx.container, ctx.dir, ctx.db, ctx.args)
  }
  object Context extends LowPriority {
    final object MissingError extends ErrorMsg (
      "Missing an implicit DFDesign Context.",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evBlockContext(
      implicit meta : Meta, container : DFOwner.Container, dir : DFDir, db : DFDesign.DB.Mutable
    ) : DFBlock.Context = new DFBlock.Context(meta, implicitly[Meta.SymbolOf[DFDesign]], container, dir, db, ClassArgs.empty)
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



