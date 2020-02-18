package ZFiant.compiler.backend.vhdl.adt

sealed trait Net extends Product with Serializable {
  val toVal : Value
  val fromVal : Value
}

object Net {
  final case class Assignment(toVal : Value, fromVal : Value) extends Net
  final case class Connection(toVal : Value, fromVal : Value) extends Net
}
