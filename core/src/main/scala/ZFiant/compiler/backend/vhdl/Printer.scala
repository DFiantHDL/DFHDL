package ZFiant
package compiler
package backend
package vhdl

sealed trait Printer {
  val getset : MemberGetSet
  val config : Printer.Config
}
object Printer {
  implicit def ev(implicit config0: Config, getset0: MemberGetSet) : Printer = new Printer {
    val getset: MemberGetSet = getset0
    val config: Config = config0
  }
  sealed trait Config {
    import io.AnsiColor._
    val LIT : String = BLUE
    val STR : String = s"\u001B[38;5;34m$BOLD"
    val KW : String = s"$BLUE$BOLD"
    final val formatter = new compiler.printer.Formatter("  ", List(25, 25))
  }
  object Config {
    implicit object Default extends Config
  }
}
