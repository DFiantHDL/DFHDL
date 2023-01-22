package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

class VerilogPrinter(using val getSet: MemberGetSet)
    extends Printer,
      VerilogTypePrinter,
      VerilogTokenPrinter,
      VerilogValPrinter,
      VerilogOwnerPrinter:
  type TPrinter = VerilogPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this VerilogPrinter."
  )
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
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csTimer(timer: Timer): String = unsupported
  def globalFileName: String = s"${printer.defsName}.vhd"
  def designFileName(designName: String): String = s"$designName.sv"
  def alignFile(csFile: String): String =
    csFile
      // align after port modifiers
      .align("[ ]*(?:input|output|inout)[ ]*", "", ".*")
      // align after wire/reg/logic words
      .align(".* (?:wire|reg|logic)[ ]*", "", ".*")
      // align signal and port names
      .align(".* (?:wire|reg).*", "", " [a-zA-Z0-9_]+,?")
      // align assignments
      .align("[ ]*[a-zA-Z0-9_]+[ ]*", "=|<=", ".*")
end VerilogPrinter