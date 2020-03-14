package ZFiant.compiler.backend.vhdl.syntax

sealed trait Net extends Product with Serializable {
  val toVal : String
  val fromVal : String
}

object Net {
  final case class Assignment(toVal : String, fromVal : String) extends Net
  final case class Connection(toVal : String, fromVal : String) extends Net
}
