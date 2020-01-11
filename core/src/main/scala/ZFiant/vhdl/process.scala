package ZFiant.vhdl

case class process(sensitivity : process.Sensitivity, variables : List[Value.Def], statements : List[statement]) extends statement

object process {
  sealed trait Sensitivity extends Product with Serializable
  object Sensitivity {
    case class List(names : scala.List[Name]) extends Sensitivity
    case object All extends Sensitivity
  }
  def sync_rst(clkName : Name, rstName : Name) : process = ???
}
