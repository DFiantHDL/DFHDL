package dfhdl.compiler.stages.verilog
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions
import DFVal.Func.Op as FuncOp
import dfhdl.compiler.ir.TextOut.Severity

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
  protected def withGetSet(subGetSet: MemberGetSet): VerilogPrinter =
    new VerilogPrinter(dialect)(using subGetSet, printerOptions)
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this VerilogPrinter."
  )
  val tupleSupportEnable: Boolean = false
  // the pre-SystemVerilog dialects have no packed (multi-dimensional) arrays
  val supportPackedArrays: Boolean =
    dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  //format: off
  /** The vector-typed declarations (variables and constants) that keep the UNPACKED array
    * representation under the SystemVerilog dialects, where a DFHDL vector prints as a PACKED
    * (descending-range) array by default. A declaration stays unpacked in two cases:
    *
    *   - a cell type this printer cannot express in a packed array (see
    *     [[supportsPackedVector]]) forces unpacked, unconditionally: a TYPE property, so every
    *     value of such a vector type agrees
    *   - otherwise, a non-global declaration whose shape/usage follows a memory access pattern
    *     (see [[dfhdl.compiler.analysis.hasMemAccessPattern]]) stays unpacked, so
    *     block-RAM/ROM inference is preserved; globals are always packed (their usage spans
    *     designs, while this analysis is design-local)
    *
    * The memory-pattern rules only admit declarations that are accessed element-by-element
    * through dynamic indexes (or via their init, which is representation-neutral), which is
    * what keeps the two representations from ever meeting in one operation.
    */
  //format: on
  lazy val unpackedVectorDcls: Set[DFVal] =
    if (!supportPackedArrays) Set.empty
    else
      getSet.designDB.members.view.flatMap {
        case dfVal: DFVal =>
          dfVal.dfType match
            case vecType: DFVector =>
              dfVal match
                case (_: DFVal.Dcl) | DclConst() =>
                  if (!supportsPackedVector(vecType)) Some(dfVal)
                  else if (dfVal.isGlobal) None
                  else Option.when(dfVal.hasMemAccessPattern)(dfVal)
                case _ => None
            case _ => None
        case _ => None
      }.toSet
  end unpackedVectorDcls

  /** Is this vector-typed declaration printed as an UNPACKED array? */
  def isUnpackedDcl(dfVal: DFVal): Boolean = unpackedVectorDcls.contains(dfVal)

  /** Is this vector-typed VALUE of the unpacked representation? Only a direct reference to an
    * unpacked declaration (or an outer-dimension slice of one) is unpacked; every expression value,
    * element select (an inner dimension), and cast result is packed.
    */
  def isUnpackedVal(dfVal: DFVal): Boolean =
    dfVal match
      case alias: DFVal.Alias.ApplyRange => isUnpackedVal(alias.relValRef.get)
      case _                             => unpackedVectorDcls.contains(dfVal)
  def csViaConnectionSep: String = ","
  def csAssignment(lhsStr: String, rhsStr: String, lhsDcl: DFVal.Dcl): String =
    s"$lhsStr = $rhsStr;"
  def csNBAssignment(lhsStr: String, rhsStr: String, lhsDcl: DFVal.Dcl): String =
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
    if (wait.isEndless) "wait(0);"
    else
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
  // elaboration system tasks ($info/$warning/$error/$fatal as module or generate items,
  // IEEE 1800-2009 par. 20.11), which is what lets a static assertion be checked by synthesis
  // and not only by simulation
  val elabTaskIsSupported: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 | VerilogDialect.sv2005 => false
      case _                                                                 => true
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
        case DFBool         => s"""$csDFVal ? "true" : "false""""
        case dfType: DFEnum =>
          if (printer.allowTypeDef) s"$csDFVal.name()"
          else s"${dfType.name}_to_string($csDFVal)"
        case _ => csDFVal
    // literal `%` must be escaped since the message is used as a display format string
    def escapeFmt(str: String): String = str.replace("%", "%%")
    // each message line is a single format string followed by its dependent value arguments
    type MsgLine = (String, List[String])
    val msgLines: List[MsgLine] =
      textOut.op match
        case TextOut.Op.Debug =>
          import textOut.meta.position as pos
          val preambleLF = if (textOut.msgArgs.nonEmpty) "\n" else ""
          val preambleLines = List(
            (escapeFmt(s"Debug at ${textOut.getOwnerDomain.getFullName}\n"), Nil),
            (escapeFmt(s"${pos.fileUnixPath}:${pos.lineStart}:${pos.columnStart}$preambleLF"), Nil)
          )
          val argLines = textOut.msgArgs.zipWithIndex.map((a, i) =>
            val argLF = if (i == textOut.msgArgs.length - 1) "" else "\n"
            (
              s"${escapeFmt(a.get.getName)} = ${csDFValToVerilogFormat(a)}$argLF",
              List(csDFValToVerilogString(a))
            )
          )
          preambleLines ++ argLines
        case _ =>
          val (completedLines, openLine) = textOut.msgParts.coalesce(textOut.msgArgs)
            .foldLeft((List.empty[MsgLine], ("", List.empty[String]))) {
              case ((lines, (fmt, args)), str: String) =>
                val segments = str.split("\n", -1)
                if (segments.length == 1) (lines, (fmt + escapeFmt(str), args))
                else
                  val headLine = (fmt + escapeFmt(segments.head) + "\n", args)
                  val midLines =
                    segments.tail.init.toList.map(s => (escapeFmt(s) + "\n", List.empty[String]))
                  (lines ++ (headLine :: midLines), (escapeFmt(segments.last), Nil))
              case ((lines, (fmt, args)), arg: DFVal.Ref) =>
                (lines, (fmt + csDFValToVerilogFormat(arg), args :+ csDFValToVerilogString(arg)))
            }
          if (openLine._1.nonEmpty || openLine._2.nonEmpty) completedLines :+ openLine
          else completedLines
    def csMsgLine(line: MsgLine): String = (scalaToVerilogString(line._1) :: line._2).mkString(", ")
    def csMsg(lines: List[MsgLine]): String =
      lines match
        case Nil         => ""
        case line :: Nil => csMsgLine(line)
        case _           => "\n" + lines.map(csMsgLine).mkString(",\n").hindent + "\n"
    val msg = csMsg(msgLines)
    def csSeverity(severity: TextOut.Severity): String =
      "$" + severity.toString.toLowerCase
    def csFinish(severity: TextOut.Severity) =
      if (severity == TextOut.Severity.Fatal) "\n$finish;" else ""
    def csDisplay(severity: TextOut.Severity, lines: List[MsgLine]) =
      val prefix = s"${severity.toString.toUpperCase()}: "
      val prefixedLines = lines match
        case (fmt, args) :: rest => (prefix + fmt, args) :: rest
        case Nil                 => List((prefix, Nil))
      s"""$$display(${csMsg(prefixedLines)});${csFinish(severity)}"""
    textOut.op match
      case TextOut.Op.Finish           => "$finish;"
      case TextOut.Op.Report(severity) =>
        if (assertIsSupported)
          val errCodeArg = severity match
            case Severity.Fatal => "1, "
            case _              => ""
          s"${csSeverity(severity)}($errCodeArg$msg);"
        else csDisplay(severity, msgLines)
      // A static assertion is a concurrent design contract, and a Verilog module body has no
      // concurrent statements: an immediate assertion and a system-task call are both statements,
      // never module items. It is checked at elaboration where the dialect has the tasks for it,
      // and at simulation time zero from an `initial` block otherwise.
      case TextOut.Op.Assert(assertionRef, severity) if textOut.isStaticAssert =>
        val cond = assertionRef.refCodeString
        val failMsg = if (msg.isEmpty) scalaToVerilogString("Assertion failed!") else msg
        // Verilog names a block, not a statement, so a named assertion becomes a named block:
        // the generate block of the elaboration form, or the `initial` block of the others.
        // Naming the generate block is also what keeps a linter from complaining about the
        // implicit `genblk<n>` the LRM would otherwise assign it.
        // an anonymous assertion keeps the tightest form the dialect allows; a named one is
        // wrapped in a named block, which is the only thing Verilog lets a name attach to
        def named(body: String): String =
          s"""|begin : ${textOut.getName}
              |${body.hindent}
              |end""".stripMargin
        if (elabTaskIsSupported)
          val errCodeArg = if (severity == TextOut.Severity.Fatal) "1, " else ""
          val task = s"${csSeverity(severity)}($errCodeArg$failMsg);"
          if (textOut.isAnonymous) s"if (!($cond)) $task"
          else s"if (!($cond)) ${named(task)}"
        else if (assertIsSupported)
          val errCodeArg = if (severity == TextOut.Severity.Fatal) "1, " else ""
          val body =
            s"""|assert ($cond)
                |else ${csSeverity(severity)}($errCodeArg$failMsg);""".stripMargin
          if (textOut.isAnonymous) s"initial\n${body.hindent}"
          else s"initial ${named(body)}"
        else
          val failLines = if (msgLines.isEmpty) List(("Assertion failed!", Nil)) else msgLines
          val body =
            s"""|if (!($cond)) begin
                |${csDisplay(severity, failLines).hindent}
                |end""".stripMargin
          if (textOut.isAnonymous) s"initial $body"
          else s"initial ${named(body)}"
        end if
      case TextOut.Op.Assert(assertionRef, severity) =>
        // a procedural assertion: 1800 allows a statement label, older dialects only a block
        val csLabel = if (textOut.isAnonymous) "" else s"${textOut.getName}: "
        val csBlockName = if (textOut.isAnonymous) "" else s" : ${textOut.getName}"
        val errCodeArg = if (severity == TextOut.Severity.Fatal) "1, " else ""
        if (msg.isEmpty)
          if (assertIsSupported) s"${csLabel}assert (${assertionRef.refCodeString});"
          else
            s"""|if (!(${assertionRef.refCodeString})) begin$csBlockName
                |${csDisplay(severity, List(("Assertion failed!", Nil))).hindent}
                |end""".stripMargin
        else if (assertIsSupported)
          s"""|${csLabel}assert (${assertionRef.refCodeString})
              |else ${csSeverity(severity)}($errCodeArg$msg);""".stripMargin
        else
          s"""|if (!(${assertionRef.refCodeString})) begin$csBlockName
              |${csDisplay(severity, msgLines).hindent}
              |end""".stripMargin
      case TextOut.Op.Print   => s"$$write($msg);"
      case TextOut.Op.Println => s"$$display($msg);"
      case TextOut.Op.Debug   =>
        if (assertIsSupported) s"$$info($msg);"
        else csDisplay(TextOut.Severity.Info, msgLines)
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
  def csAnnotations(annotations: List[annotation.HWAnnotation]): String = ""
  // def csTimer(timer: Timer): String = unsupported
  def verilogFileHeaderSuffix: String =
    printer.dialect match
      case VerilogDialect.v2001 | VerilogDialect.v95 => "vh"
      case _                                         => "svh"
  override def csGlobalMemberQualifier(ns: String, pkgName: String): String = s"$pkgName::"
  override def supportPackages: Boolean =
    printer.dialect match
      case VerilogDialect.v95 | VerilogDialect.v2001 => false
      case _                                         => true
  override def packageFileName(pkgName: String): String = s"$pkgName.sv"
  override def csPackageFileContent(pkgName: String, namespace: String, typeDcls: String): String =
    // A package file carries the same file header as a design file. The directives are
    // COMPILATION-UNIT state rather than file state, so a file that omits them inherits
    // whatever the previously compiled file left behind, and packages compile ahead of the
    // designs. `csLibrary` also emits the global defs include, which packaged type
    // declarations may reference (e.g. a struct field of a global-placed named type).
    sn"""|${csLibrary(getSet.designDB.inSimulation, minTimeUnitGlobalOpt)}
        |
        |package $pkgName;
        |$typeDcls
        |endpackage
        |"""
  def globalFileName: String =
    val name = printerOptions.globalDefsFileName
    if (name.nonEmpty && name.contains('.')) name
    else s"${printer.defsName}.$verilogFileHeaderSuffix"
  override def csGlobalFileContent: String =
    if (hasGlobalContent)
      val defName = printer.defsName.toUpperCase
      // the module defs are alternating between outside of and inside of the module
      // because we will include the module defs twice, once in the top of the file
      // and second time inside the module.
      val globalParams =
        if (printer.supportGlobalParameters) super.csGlobalFileContent else ""
      val globalToLocalParams =
        if (printer.supportGlobalParameters) "" else super.csGlobalFileContent
      val moduleDefs =
        if (printer.allowTypeDef) ""
        else
          sn"""|`ifndef ${defName}_MODULE
              |`define ${defName}_MODULE
              |`else
              |$globalToLocalParams
              |${printer.csGlobalTypeFuncDcls}
              |`undef ${defName}_MODULE
              |`endif"""
      sn"""|`ifndef $defName
          |`define $defName
          |$globalParams
          |`endif
          |$moduleDefs
          |"""
    else ""
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
      .align("[ ]*(?:parameter|localparam) [a-zA-Z0-9_.]+[ ]*", "=", ".*;")
      // align enum constants
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*", "=", "[ ]*[0-9]+,?")
      // align cases
      .align("[ ]*[a-zA-Z]+[a-zA-Z0-9_.]*[ ]*:[ ]*", "", ".*")
  end alignCode

  val verilogKW: Set[String] = Set(
    "module", "input", "output", "inout", "endmodule", "always", "always_comb", "always_ff",
    "begin", "end", "case", "default", "endcase", "default_nettype", "include", "initial", "inside",
    "timescale", "if", "else", "typedef", "enum", "posedge", "negedge", "assign", "parameter",
    "struct", "packed", "ifndef", "endif", "define", "function", "endfunction", "for", "while",
    "assert", "write", "display", "info", "warning", "error", "fatal", "finish", "localparam",
    "unique", "task", "automatic", "endtask", "ref"
  )
  val verilogOps: Set[String] = Set("=", "<=")
  val verilogTypes: Set[String] = Set(
    "wire", "reg", "logic", "wire", "signed", "unsigned", "int", "integer", "string", "real"
  )
  def colorCode(cs: String): String =
    cs
      .colorWords(verilogKW, keywordColor)
      .colorOps(verilogOps, keywordColor)
      .colorWords(verilogTypes, typeColor)
      .colorLineComment("//", commentColor)
      .colorBlockComment("/\\*", "\\*/", commentColor)

end VerilogPrinter
