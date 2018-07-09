package DFiant

import singleton.ops._

case class PortNode (
  dfport : DFAny.Port[DFAny, DFPort.DFDir],
  name : String
) {
  override def toString: String = name
}

object DFPort {
  type <>[DF <: DFAny, DIR <: DFDir] = DFAny.Port[DF, DIR] with DF
  //Direction of a Port
  sealed trait DFDir {
    val isOut : Boolean
    val isIn : Boolean
  }
  sealed trait IN extends DFDir {
    override def toString: String = "IN"
    final val isOut : Boolean = false
    final val isIn : Boolean = true
  }
  sealed trait DefaultIN[T] extends IN {
    val default : T
  }
  implicit object IN extends IN {
    def apply[T](default : T)(implicit g : GetArg.GetArg[0]) : DefaultIN[g.Out] = new DefaultIN[g.Out] {
      val default = g.value
    }
  }
  sealed trait OUT extends DFDir {
    override def toString: String = "OUT"
    final val isOut : Boolean = true
    final val isIn : Boolean = false
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

