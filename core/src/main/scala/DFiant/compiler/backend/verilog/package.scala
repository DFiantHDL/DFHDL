package DFiant
package compiler
package backend

package object verilog {
  private[verilog] val reservedKeywords : Set[String] = Set(
    "always", "end", "ifnone", "or", "rpmos", "tranif1", "and", "endcase", "initial", "output",
    "rtran", "tri", "assign", "endmodule", "inout", "parameter", "rtranif0", "tri0", "begin", "endfunction", "input", "pmos",
    "rtranif1", "tri1", "buf", "endprimitive", "integer", "posedge", "scalared", "triand", "bufif0", "endspecify", "join", "primitive",
    "small", "trior", "bufif1", "endtable", "large", "pull0", "specify", "trireg", "case", "endtask", "macromodule",
    "pull1", "specparam", "vectored", "casex", "event", "medium", "pullup", "strong0", "wait", "casez", "for", "module", "pulldown",
    "strong1", "wand", "cmos", "force", "nand", "rcmos", "supply0", "weak0", "deassign", "forever", "negedge", "real",
    "supply1", "weak1", "default", "for", "nmos", "realtime", "table", "while", "defparam", "function", "nor", "reg", "task", "wire", "disable", "highz0",
    "not", "release", "time", "wor", "edge", "highz1", "notif0", "repeat", "tran", "xnor", "else", "if", "notif1", "rnmos", "tranif0", "xor"
  )

  private implicit def compiler[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : Compiler[D] = new Compiler[D](c)

  sealed trait Revision extends Product with Serializable
  object Revision {
    implicit case object V95 extends Revision
    type V95 = V95.type
    implicit case object V2005 extends Revision
    type V2005 = V2005.type
  }

  trait Backend[R <: Revision] extends BackendStage {
    def codeString : String = "verilog"
    val revision : R
  }
  object Backend extends Backend[Revision] {
    lazy val revision : Revision = ???
  }

  implicit object v95 extends BackendStage.Compiler[Backend[Revision.V95]] {
    def apply[D <: DFDesign](c : IRCompilation[D]) : BackendStage.Compilation[D, Backend[Revision.V95]] = c.verilogCompile[Revision.V95]
  }
  implicit object v2005 extends BackendStage.Compiler[Backend[Revision.V2005]] {
    def apply[D <: DFDesign](c : IRCompilation[D]) : BackendStage.Compilation[D, Backend[Revision.V2005]] = c.verilogCompile[Revision.V2005]
  }
  private[verilog] type Printer = DFiant.printer.Printer[Printer.Config]
  private[verilog] object Printer {
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
