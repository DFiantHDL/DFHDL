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
  val commentConnDir: CommentConnDir = CommentConnDir.Inline
  def csAssignmentOp: String = "="
  def csNBAssignmentOp: String = "<="
  def csConnectionOp: String = "<="
  def csLateConnectionOp: String = ""
  def csLateConnectionSep: String = ","
  def csLazyConnectionOp: String = unsupported
  final val normalizeLateConnection: Boolean = true
  final val normalizeConnection: Boolean = true
  def csInternalViaPortRef(dfValRef: DFNet.Ref): String = s".${dfValRef.refCodeString}"
  def csExternalViaPortRef(dfValRef: DFNet.Ref): String = s"(${dfValRef.refCodeString})"
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.indent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csEndOfStatement: String = ";"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csTimer(timer: Timer): String = unsupported
end VerilogPrinter
