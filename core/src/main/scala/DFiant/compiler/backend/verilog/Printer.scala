package DFiant
package compiler.backend.verilog

object Printer {
  sealed class Config(val revision: Revision) extends DFiant.printer.Printer.Config {
    import io.AnsiColor._
    val DELIM : String = "  "
    val maxAlignments : List[Int] = List(25, 25)
    val LIT : String = "\u001B[38;5;5m"
    val KW : String = s"$BLUE$BOLD"
    val OP : String = s"$BOLD"
    val FN : String = "\u001B[38;5;54m"
    val TP : String = "\u001B[38;5;94m"
  }
}
