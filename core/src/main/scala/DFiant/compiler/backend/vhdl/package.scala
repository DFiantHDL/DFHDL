package DFiant
package compiler
package backend

import shapeless.HList

package object vhdl {
  private[vhdl] val reservedKeywords : Set[String] = Set(
    "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute", "begin",
    "block", "body", "buffer", "bus", "case", "component", "configuration", "constant", "disconnect", "downto",
    "else", "elsif", "end", "entity", "exit", "file", "for", "function", "generate", "generic", "group",
    "guarded", "if", "impure", "in", "inertial", "inout", "is", "label", "library", "linkage", "literal", "loop",
    "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open", "or", "others", "out",
    "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register", "reject",
    "rem", "report", "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sll", "sra",
    "srl", "subtype", "then", "to", "transport", "type", "unaffected", "units", "until", "use", "variable",
    "wait", "when", "while", "with", "xnor", "xor",
  )

  private implicit def compiler[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, S])
  : Compiler[D, S] = new Compiler[D, S](c)

  sealed trait Revision extends Product with Serializable
  object Revision {
    implicit case object V93 extends Revision
    type V93 = V93.type
    implicit case object V2008 extends Revision
    type V2008 = V2008.type
  }

  trait Backend[R <: Revision] extends BackendStage {
    def codeString : String = "vhdl"
    val revision : R
  }
  object Backend extends Backend[Revision] {
    lazy val revision : Revision = ???
  }

  implicit object v93 extends BackendStage.Compiler[Backend[Revision.V93]] {
    def apply[D <: DFDesign, H <: HList](c : IRCompilation[D, H]) : BackendStage.Compilation[D, Backend[Revision.V93]] = c.vhdlCompile[Revision.V93]
  }
  implicit object v2008 extends BackendStage.Compiler[Backend[Revision.V2008]] {
    def apply[D <: DFDesign, H <: HList](c : IRCompilation[D, H]) : BackendStage.Compilation[D, Backend[Revision.V2008]] = c.vhdlCompile[Revision.V2008]
  }
  private[vhdl] type Printer = DFiant.printer.Printer[Printer.Config]
  private[vhdl] object Printer {
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
}
