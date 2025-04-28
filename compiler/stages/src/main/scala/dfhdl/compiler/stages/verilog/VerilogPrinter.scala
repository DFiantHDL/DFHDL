package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions
import DFVal.Func.Op as FuncOp

class VerilogPrinter(val dialect: VerilogDialect)(using
    val getSet: MemberGetSet,
    val printerOptions: PrinterOptions
) extends Printer,
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
  def csAssignment(lhsStr: String, rhsStr: String, shared: Boolean): String =
    val cs = s"$lhsStr = $rhsStr;"
    if (shared)
      s"""|/* verilator lint_off BLKSEQ */
          |$cs
          |/* verilator lint_on BLKSEQ */""".stripMargin
    else cs
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
  def csGoto(goto: Goto): String = unsupported
  def csDFRange(range: DFRange): String = unsupported
  def csWait(wait: Wait): String =
    val trigger = wait.triggerRef.get
    trigger.dfType match
      case _: DFBoolOrBit =>
        trigger match
          case DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
            s"@(${wait.triggerRef.refCodeString});"
          case _ =>
            s"wait(${wait.triggerRef.refCodeString});"
      case DFTime => s"#${wait.triggerRef.refCodeString};"
      case _      => printer.unsupported
  val assertIsSupported: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  def csTextOut(textOut: TextOut): String =
    def csDFValToVerilogFormat(dfValRef: DFVal.Ref): String =
      dfValRef.get.dfType match
        case DFXInt(_, _, _) => s"%d"
        case DFBits(_)       => s"%h"
        case DFBit           => s"%b"
        case _               => s"%s"
    def csDFValToVerilogString(dfValRef: DFVal.Ref): String =
      val dfVal = dfValRef.get
      val csDFVal = dfValRef.refCodeString
      dfVal.dfType match
        case DFBool => s"""$csDFVal ? "true" : "false""""
        case dfType: DFEnum =>
          if (printer.allowTypeDef) s"$csDFVal.name()"
          else s"${dfType.getName}_to_string($csDFVal)"
        case _ => csDFVal
    val msg =
      textOut.op match
        case TextOut.Op.Debug =>
          import textOut.meta.position as pos
          val preambleLF = if (textOut.msgArgs.nonEmpty) "\n" else ""
          //format: off
          val preamble =
            s"""|${scalaToVerilogString(s"Debug at ${textOut.getOwnerDomain.getFullName}\n")},
                |${scalaToVerilogString(s"${pos.fileUnixPath}:${pos.lineStart}:${pos.columnStart}$preambleLF")}""".stripMargin
          //format: on
          val args =
            if (textOut.msgArgs.isEmpty) ""
            else
              textOut.msgArgs.view.zipWithIndex.map((a, i) =>
                val argLF = if (i == textOut.msgArgs.length - 1) "" else "\n"
                s"${scalaToVerilogString(s"${a.get.getName} = ${csDFValToVerilogFormat(a)}$argLF")}, ${csDFValToVerilogString(a)}"
              ).mkString(",\n", ",\n", "")
          "\n" + (preamble + args).hindent + "\n"
        case _ =>
          val allParts = textOut.msgParts.coalesce(textOut.msgArgs).flatMap {
            case str: String if str.contains("\n") =>
              val strs = str.split("\n")
              if (strs.last.isEmpty) strs.dropRight(1).map(_ + "\n")
              else strs.dropRight(1).map(_ + "\n") :+ strs.last
            case p =>
              List(p)
          }.toList
          if (allParts.isEmpty) ""
          else if (allParts.length == 1) scalaToVerilogString(allParts.head.asInstanceOf[String])
          else
            val unindented = allParts.lazyZip(allParts.tail :+ "").map {
              case (part: String, arg: DFVal.Ref) =>
                if (part.endsWith("\n"))
                  s"${scalaToVerilogString(part.dropRight(1) + csDFValToVerilogFormat(arg) + "\n")}, ${csDFValToVerilogString(arg)},\n"
                else
                  s"${scalaToVerilogString(part + csDFValToVerilogFormat(arg))}, ${csDFValToVerilogString(arg)}, "
              case (part: String, next: String) =>
                if (next.isEmpty) scalaToVerilogString(part)
                else if (part.endsWith("\n")) scalaToVerilogString(part) + ",\n"
                else scalaToVerilogString(part) + ", "
              case _ => ""
            }.mkString
            if (unindented.contains("\n")) "\n" + unindented.hindent + "\n"
            else unindented
          end if
      end match
    end msg
    def csSeverity(severity: TextOut.Severity): String =
      "$" + severity.toString.toLowerCase
    def csFinish(severity: TextOut.Severity) =
      if (severity == TextOut.Severity.Fatal) "\n$finish;" else ""
    def csDisplay(severity: TextOut.Severity, msg: String) =
      s"""$$display("${severity.toString.toUpperCase()}: ", $msg);${csFinish(severity)}"""
    textOut.op match
      case TextOut.Op.Finish => "$finish;"
      case TextOut.Op.Report(severity) =>
        if (assertIsSupported) s"${csSeverity(severity)}($msg);"
        else csDisplay(severity, msg)
      case TextOut.Op.Assert(assertionRef, severity) =>
        if (msg.isEmpty)
          if (assertIsSupported) s"assert (${assertionRef.refCodeString});"
          else
            s"""|if (!(${assertionRef.refCodeString})) begin
                |${csDisplay(severity, "\"Assertion failed!\"").hindent}
                |end""".stripMargin
        else if (assertIsSupported)
          s"""|assert (${assertionRef.refCodeString})
              |else ${csSeverity(severity)}($msg);""".stripMargin
        else
          s"""|if (!(${assertionRef.refCodeString})) begin
              |${csDisplay(severity, msg).hindent}
              |end""".stripMargin
      case TextOut.Op.Print   => s"$$write($msg);"
      case TextOut.Op.Println => s"$$display($msg);"
      case TextOut.Op.Debug =>
        if (assertIsSupported) s"$$info($msg);"
        else csDisplay(TextOut.Severity.Info, msg)
    end match
  end csTextOut
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csDocString(doc: String): String = doc.betterLinesIterator.mkString("/*", "\n  ", "*/")
  def csAnnotations(meta: Meta): String = ""
  // def csTimer(timer: Timer): String = unsupported
  def verilogFileHeaderSuffix: String =
    printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => "vh"
      case _                                         => "svh"
  def globalFileName: String =
    s"${printer.defsName}.$verilogFileHeaderSuffix"
  override def csGlobalFileContent: String =
    val defName = printer.defsName.toUpperCase
    // the module defs are alternating between outside of and inside of the module
    // because we will include the module defs twice, once in the top of the file
    // and second time inside the module.
    val moduleDefs =
      if (printer.allowTypeDef) ""
      else
        s"""|
            |`ifndef ${defName}_MODULE
            |`define ${defName}_MODULE
            |`else
            |${printer.csGlobalTypeFuncDcls}
            |`undef ${defName}_MODULE
            |`endif
            |""".stripMargin
    s"""`ifndef $defName
       |`define $defName
       |${super.csGlobalFileContent}
       |`endif$moduleDefs
       |""".stripMargin
  end csGlobalFileContent
  def dfhdlDefsFileName: String = s"dfhdl_defs.$verilogFileHeaderSuffix"
  def dfhdlSourceContents: String =
    scala.io.Source.fromResource(dfhdlDefsFileName).getLines().mkString("\n")

  def designFileName(designName: String): String =
    val suffix = printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => "v"
      case _                                         => "sv"
    s"$designName.$suffix"
  def alignCode(cs: String): String =
    cs
      // align logic position after port direction
      .align("[ ]*(?:input|output|inout).*", " logic ", ".*")
      // align port names
      .align("[ ]*(?:input|output|inout).*", " ", "[a-zA-Z0-9_.]+[,;]?")
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
      .align("[ ]*[a-zA-Z0-9_.\\[\\]\\:]+[ ]*", "=|<=", ".*;")
      // align connections (verilog assignments)
      .align("[ ]*assign [a-zA-Z0-9_.\\[\\]\\:]+[ ]*", "=", ".*;")
      // align parameters
      .align("[ ]*parameter [a-zA-Z0-9_.]+[ ]*", "=", ".*;")
      // align enum constants
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*", "=", "[ ]*[0-9]+,?")
      // align cases
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*:", "", ".*")

  val verilogKW: Set[String] =
    Set("module", "input", "output", "inout", "endmodule", "always", "always_comb", "always_ff",
      "begin", "end", "case", "default", "endcase", "default_nettype", "include", "inside",
      "timescale", "if", "else", "typedef", "enum", "posedge", "negedge", "assign", "parameter",
      "struct", "packed", "ifndef", "endif", "define", "function", "endfunction", "for", "while",
      "assert", "write", "display", "info", "warning", "error", "fatal")
  val verilogOps: Set[String] = Set("=", "<=")
  val verilogTypes: Set[String] =
    Set("wire", "reg", "logic", "wire", "signed", "int", "integer", "string")
  def colorCode(cs: String): String =
    cs
      .colorWords(verilogKW, keywordColor)
      .colorOps(verilogOps, keywordColor)
      .colorWords(verilogTypes, typeColor)
      .colorLineComment("//", commentColor)
      .colorBlockComment("/\\*", "\\*/", commentColor)

end VerilogPrinter
