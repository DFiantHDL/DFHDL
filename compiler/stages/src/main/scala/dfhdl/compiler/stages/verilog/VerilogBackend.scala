package dfhdl.compiler.stages.verilog

import dfhdl.compiler.stages.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.printing.*
import java.nio.file.{Files, Paths}
import java.io._

private val reservedKeywords: Set[String] = Set(
  "always", "end", "ifnone", "or", "rpmos", "tranif1", "and", "endcase", "initial", "output",
  "cell", "rtran", "tri", "assign", "endmodule", "inout", "parameter", "rtranif0", "tri0", "begin",
  "endfunction", "input", "pmos", "rtranif1", "tri1", "buf", "endprimitive", "integer", "posedge",
  "scalared", "triand", "bufif0", "endspecify", "join", "primitive", "small", "trior", "bufif1",
  "endtable", "large", "pull0", "specify", "trireg", "case", "endtask", "macromodule", "pull1",
  "specparam", "vectored", "casex", "event", "medium", "pullup", "strong0", "wait", "casez", "for",
  "module", "pulldown", "strong1", "wand", "cmos", "force", "nand", "rcmos", "supply0", "weak0",
  "deassign", "forever", "negedge", "real", "supply1", "weak1", "default", "for", "nmos",
  "realtime", "table", "while", "defparam", "function", "nor", "reg", "task", "wire", "disable",
  "highz0", "not", "release", "time", "wor", "edge", "highz1", "notif0", "repeat", "tran", "xnor",
  "else", "if", "notif1", "rnmos", "tranif0", "xor"
)

private case object VerilogUniqueNames extends UniqueNames(reservedKeywords, caseSensitive = true)
case object VerilogBackend extends Stage:
  def dependencies: List[Stage] =
    List(ToED, ExplicitNamedVars, SimpleOrderMembers, VerilogUniqueNames, ViaConnection)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
end VerilogBackend

extension [T: HasDB](t: T)
  def getVerilogCode(align: Boolean): String =
    val designDB = StageRunner.run(VerilogBackend)(t.db)
    designDB.printCodeString
    val printer = new VerilogPrinter(using designDB.getSet):
      override val alignEnable: Boolean = align
    printer.csDB
  end getVerilogCode
  def getVerilogCode: String = getVerilogCode(align = false)
end extension
