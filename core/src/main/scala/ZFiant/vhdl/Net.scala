package ZFiant.vhdl

sealed trait Net extends Product with Serializable {
  val toVal : Value
  val fromVal : Value
}

object Net {
  case class Assignment(toVal : Value, fromVal : Value) extends Net
  case class Connection(toVal : Value, fromVal : Value) extends Net
}
