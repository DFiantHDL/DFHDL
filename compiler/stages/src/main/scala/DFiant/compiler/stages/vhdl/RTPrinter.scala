package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*

class RTPrinter(using val getSet: MemberGetSet)
    extends Printer,
      RTTypePrinter,
      RTTokenPrinter,
      RTValPrinter,
      RTOwnerPrinter:
  type TPrinter = RTPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
  val commentConnDir: CommentConnDir = CommentConnDir.Inline
  val csAssignmentOp: String = ":="
  val csConnectionOp: String = "<="
  val csLateConnectionOp: String = "=>"
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
end RTPrinter
