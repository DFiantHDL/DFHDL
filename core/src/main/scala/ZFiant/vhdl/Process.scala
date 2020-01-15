package ZFiant.vhdl

case class Process(sensitivity : Process.Sensitivity, variables : List[Value.Def.Modifier.Port], statements : List[Statement]) extends Statement

object Process {
  sealed trait Sensitivity extends Product with Serializable
  object Sensitivity {
    case class List(names : scala.List[Name]) extends Sensitivity
    case object All extends Sensitivity
  }
  def sync_rst(clkName : Name, rstName : Name) : Process = ???
}
