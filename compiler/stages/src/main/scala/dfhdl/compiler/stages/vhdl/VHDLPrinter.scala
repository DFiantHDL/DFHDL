package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions

class VHDLPrinter(using val getSet: MemberGetSet, val printerOptions: PrinterOptions)
    extends Printer,
      VHDLTypePrinter,
      VHDLDataPrinter,
      VHDLValPrinter,
      VHDLOwnerPrinter:
  type TPrinter = VHDLPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
  val tupleSupportEnable: Boolean = false
  def csViaConnectionSep: String = ","
  def csAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr := $rhsStr;"
  def csNBAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr <= $rhsStr;"
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr <= $rhsStr;"
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr => $rhsStr"
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    unsupported
  final val normalizeViaConnection: Boolean = true
  final val normalizeConnection: Boolean = true
  def csOpenKeyWord: String = "open"
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"-- $comment"
  def csDocString(doc: String): String = doc.linesIterator.mkString("--", "\n--", "")
  def csAnnotations(meta: Meta): String = ""
  def csTimer(timer: Timer): String = unsupported
  def globalFileName: String = s"${printer.packageName}.vhd"
  def designFileName(designName: String): String = s"$designName.vhd"
  def alignCode(cs: String): String =
    cs
      .align(".*", ":", "[ ]*(?:in|out|inout) .*")
      .align(".*:[ ]*(?:in|out|inout)", " ", ".*")
      .align("[ ]*(?:signal|variable|constant) .*", ": ", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<=", ".*")
  def colorCode(cs: String): String =
    cs
end VHDLPrinter
