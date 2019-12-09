package ZFiant

sealed trait DFNet extends DFMember

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(to : DFAny.Var[_ <: DFAny.Type], from : DFAny)(
    implicit val ctx : DFNet.Context
  ) extends DFNet
  final case class Connection(left : DFAny, right : DFAny)(
    implicit val ctx : DFNet.Context
  ) extends DFNet
}
