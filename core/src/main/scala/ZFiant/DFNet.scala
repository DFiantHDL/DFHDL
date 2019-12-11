package ZFiant
import DFiant.internals._

sealed trait DFNet extends DFMember

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(toRef : DFRef[DFAny], fromRef : DFRef[DFAny], ownerRef: DFRef[DFBlock], meta: Meta) extends DFNet {
    override def toString: String = s"${toRef.fullName} := ${fromRef.fullName}"
  }
  object Assignment {
    def apply(to: DFAny, from: DFAny)(implicit ctx: Context)
    : Assignment = ctx.compiler.addMember(Assignment(DFRef(to), from, ctx.owner, ctx.meta))
  }

  final case class Connection(leftRef : DFRef[DFAny.Connectable[_<: DFAny.Type, _]], rightRef : DFRef[DFAny], ownerRef: DFRef[DFBlock], meta: Meta) extends DFNet {
    override def toString: String = s"${leftRef.fullName} <> ${rightRef.fullName}"
  }
  object Connection {
    def apply(left: DFAny.Connectable[_ <: DFAny.Type, _ <: DFAny.Modifier], right: DFAny)(implicit ctx: Context)
    : Connection = ctx.compiler.addMember(Connection(DFRef(left), right, ctx.owner, ctx.meta))
  }
}
