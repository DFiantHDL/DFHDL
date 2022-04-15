package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*

class VHDLPrinter(using val getSet: MemberGetSet)
    extends Printer,
      VHDLTypePrinter,
      VHDLTokenPrinter,
      VHDLValPrinter,
      VHDLOwnerPrinter:
  type TPrinter = VHDLPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
  val commentConnDir: CommentConnDir = CommentConnDir.Inline
  def csAssignmentOp: String = ":="
  def csNBAssignmentOp: String = "<="
  def csConnectionOp: String = "<="
  def csLateConnectionOp: String = "=>"
  def csLateConnectionSep: String = ","
  def csLazyConnectionOp: String = unsupported
  final val normalizeLateConnection: Boolean = true
  final val normalizeConnection: Boolean = true
  def csInternalViaPortRef(dfValRef: DFNet.Ref): String = dfValRef.refCodeString
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.indent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csEndOfStatement: String = ";"
  def csCommentEOL(comment: String): String = s"-- $comment"
  def csTimer(timer: Timer): String = unsupported
end VHDLPrinter
