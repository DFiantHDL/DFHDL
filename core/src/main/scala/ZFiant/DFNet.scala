package ZFiant
import DFiant.internals._

sealed trait DFNet extends DFMember

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(to : DFAny.Var[_ <: DFAny.Type], from : DFAny, ownerRef: DFRef[DFBlock], meta: Meta) extends DFNet
  object Assignment {
    def apply(to: DFAny.Var[_ <: DFAny.Type], from: DFAny)(implicit ctx: Context)
    : Assignment = ctx.compiler.addMember(Assignment(to, from, ctx.owner, ctx.meta))
  }

  final case class Connection(left : DFAny, right : DFAny, ownerRef: DFRef[DFBlock], meta: Meta)(
    implicit val ctx : DFNet.Context
  ) extends DFNet
  object Connection {
    def apply(left: DFAny, right: DFAny)(implicit ctx: Context)
    : Connection = ctx.compiler.addMember(Connection(left, right, ctx.owner, ctx.meta))
  }
}
