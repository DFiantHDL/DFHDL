package DFiant
package compiler
package backend
package vhdl

sealed trait Printer {
  val getSet : MemberGetSet
  val config : Printer.Config
}
object Printer {
  implicit def ev(implicit config0: Config, getset0: MemberGetSet) : Printer = new Printer {
    val getSet: MemberGetSet = getset0
    val config: Config = config0
  }
  sealed trait Config {
    import io.AnsiColor._
    val LIT : String = "\u001B[38;5;5m"
    val KW : String = s"$BLUE$BOLD"
    val OP : String = s"$BOLD"
    val FN : String = "\u001B[38;5;54m"
    val TP : String = "\u001B[38;5;94m"
    final val formatter = new compiler.printer.Formatter("  ", List(25, 25))
  }
  object Config {
    implicit object Default extends Config
  }
}
