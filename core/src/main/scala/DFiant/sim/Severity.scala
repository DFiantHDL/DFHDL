package DFiant.sim

import DFiant.compiler.printer.Printer

sealed trait Severity extends Product with Serializable {
  def codeString(implicit printer: Printer) : String = {
    import printer.config._
    s"$DF sim.$DF ${this.toString}"
  }
}
case object Note extends Severity
case object Warning extends Severity
case object Error extends Severity
case object Failure extends Severity
