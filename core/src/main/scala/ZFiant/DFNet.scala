package ZFiant

sealed trait DFNet extends DFMember {
  val to : DFAny
  val from : DFAny
}

object DFNet {
  type Context = DFAny.Context

  final case class Assignment(to : DFAny.ValOrVar[_ <: DFType, true], from : DFAny)(
    implicit val ctx : DFNet.Context
  ) extends DFNet
  final case class Connection(to : DFAny.Port[_ <: DFType,_], from : DFAny)(
    implicit val ctx : DFNet.Context
  ) extends DFNet
}
