package DFiant

case class PortNode (
  dfport : DFAny.Port[DFAny, DFPort.DFDir],
  name : String
) {
  override def toString: String = name
}

object DFPort {
  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //Direction of a Port
  sealed trait DFDir
  sealed trait IN extends DFDir {
    override def toString: String = "IN"
  }
  implicit object IN extends IN
  sealed trait OUT extends DFDir {
    override def toString: String = "OUT"
  }
  implicit object OUT extends OUT

  trait Connection[+DF <: DFAny] extends Serializable
  final case class FullyConnected[+DF <: DFAny](dfVar : DF) extends Connection[DF]
  case object OPEN extends Connection[Nothing]
  type OPEN = OPEN.type
  trait TOP
  object TOP extends TOP {
    final case class Width(width : Int) extends TOP with Connection[Nothing]
  }
}

