package DFiant
package sim

import compiler.csprinter.CSPrinter

sealed trait Severity extends Product with Serializable {
  def codeString(implicit printer: CSPrinter): String = {
    import printer.config._
    s"$DF sim.$DF ${this.toString}"
  }
}
case object Note    extends Severity
case object Warning extends Severity
case object Error   extends Severity
case object Failure extends Severity
