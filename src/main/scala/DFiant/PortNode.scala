package DFiant

case class PortNode (
  dfport : DFAny.Port[DFAny, DFDir],
  name : String
) {
  override def toString: String = name
}



