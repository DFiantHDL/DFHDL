package ZFiant

sealed abstract class DFNet(to : DFAny, from : DFAny) extends DFAnyMember

object DFNet {

  final case class Assignment(to : DFAny.Constructor[_ <: DFType, true], from : DFAny) extends DFNet(to, from)
  final case class Connection(to : DFAny.Port[_ <: DFType,_], from : DFAny) extends DFNet(to, from)
}
