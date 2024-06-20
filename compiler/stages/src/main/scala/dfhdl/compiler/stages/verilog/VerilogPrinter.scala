package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions

class VerilogPrinter(using val getSet: MemberGetSet, val printerOptions: PrinterOptions)
    extends Printer,
      VerilogTypePrinter,
      VerilogDataPrinter,
      VerilogValPrinter,
      VerilogOwnerPrinter:
  type TPrinter = VerilogPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this VerilogPrinter."
  )
  val tupleSupportEnable: Boolean = false
  def csViaConnectionSep: String = ","
  def csAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr = $rhsStr;"
  def csNBAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr <= $rhsStr;"
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"assign $lhsStr = $rhsStr;"
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s".$lhsStr /*$directionStr*/ ($rhsStr)"
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    unsupported
  final val normalizeViaConnection: Boolean = true
  final val normalizeConnection: Boolean = true
  def csOpenKeyWord: String = "/*open*/"
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csDocString(doc: String): String = doc.betterLinesIterator.mkString("/*", "\n  ", "*/")
  def csAnnotations(meta: Meta): String = ""
  def csTimer(timer: Timer): String = unsupported
  def globalFileName: String = s"${printer.defsName}.sv"
  override def csGlobalFileContent: String =
    val defName = printer.defsName.toUpperCase
    s"""`ifndef $defName
       |`define $defName
       |`define MAX(a,b) ((a) > (b) ? (a) : (b))
       |`define MIN(a,b) ((a) < (b) ? (a) : (b))
       |${super.csGlobalFileContent}
       |`endif
       |""".stripMargin

  def designFileName(designName: String): String = s"$designName.sv"
  def alignCode(cs: String): String =
    cs
      // align after port modifiers
      .align("[ ]*(?:input|output|inout)", " ", ".*")
      // align after wire/reg/logic words
      .align(
        "\\s*(?:logic(?: signed)?\\s*\\[\\d+:\\d+]|[\\w]+)",
        " ",
        "[a-zA-Z0-9_]+[^=<]*;",
        !verilogKW.contains(_)
      )
//      // align signal and port names
//      .align(".* (?:wire|reg|logic).*", "", " [a-zA-Z0-9_]+.*")
      // align via connections
      .align(".*", "\\/\\*<--\\*\\/|\\/\\*-->\\*\\/", ".*")
      // align assignments
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*", "=|<=", ".*")
      // align connections (verilog assignments)
      .align("[ ]*assign [a-zA-Z0-9_.]+[ ]*", "=", ".*")
      // align parameters
      .align("[ ]*parameter [a-zA-Z0-9_.]+[ ]*", "=", ".*")
      // align enum constants
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*", "=", ".*")
      // align cases
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*:", "", ".*")

  val verilogKW: Set[String] =
    Set("module", "input", "output", "inout", "endmodule", "always", "begin", "end", "case",
      "default", "endcase", "default_nettype", "include", "timescale", "if", "else", "typedef",
      "enum", "posedge", "negedge", "assign", "parameter", "struct", "packed", "ifndef", "endif",
      "define")
  val verilogOps: Set[String] = Set("=", "<=")
  val verilogTypes: Set[String] =
    Set("wire", "reg", "logic", "wire", "signed", "int")
  def colorCode(cs: String): String =
    cs
      .colorWords(verilogKW, keywordColor)
      .colorOps(verilogOps, keywordColor)
      .colorWords(verilogTypes, typeColor)

end VerilogPrinter
