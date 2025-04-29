package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions
import scala.collection.mutable
import scala.collection.immutable.ListSet
import DFVal.Func.Op as FuncOp

class VHDLPrinter(val dialect: VHDLDialect)(using
    val getSet: MemberGetSet,
    val printerOptions: PrinterOptions
) extends Printer,
      VHDLTypePrinter,
      VHDLDataPrinter,
      VHDLValPrinter,
      VHDLOwnerPrinter:
  type TPrinter = VHDLPrinter
  given printer: TPrinter = this
  val inVHDL93: Boolean = dialect match
    case VHDLDialect.v93 => true
    case _               => false
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
  val tupleSupportEnable: Boolean = false
  def csViaConnectionSep: String = ","
  def csAssignment(lhsStr: String, rhsStr: String, shared: Boolean): String =
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
  def csGoto(goto: Goto): String = unsupported
  def csDFRange(range: DFRange): String = unsupported
  def csWait(wait: Wait): String =
    val trigger = wait.triggerRef.get
    trigger.dfType match
      case _: DFBoolOrBit =>
        trigger match
          // rising or falling edge does not need to be negated
          case DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
            s"wait until ${wait.triggerRef.refCodeString};"
          // no need for `not not`, so just skipping the not operation
          case DFVal.Func(op = FuncOp.unary_!, args = List(triggerRef)) =>
            s"wait until ${printer.csFixedCond(triggerRef)};"
          case _ =>
            s"wait until not ${printer.csFixedCond(wait.triggerRef)};"
      case DFTime => s"wait for ${wait.triggerRef.refCodeString};"
      case _      => printer.unsupported
  end csWait
  def csTextOut(textOut: TextOut): String =
    def csDFValToVHDLString(dfValRef: DFVal.Ref): String =
      val dfVal = dfValRef.get
      val csDFVal = dfValRef.refCodeString
      dfVal.dfType match
        case DFString       => csDFVal
        case dfType: DFEnum => s"${printer.csDFEnumTypeName(dfType)}'image($csDFVal)"
        case _              => s"to_string($csDFVal)"
    val msg =
      textOut.op match
        case TextOut.Op.Debug =>
          import textOut.meta.position as pos
          val preambleLF = if (textOut.msgArgs.nonEmpty) " & LF &" else ""
          //format: off
          val preamble =
            s"""|${scalaToVHDLString(s"Debug at ${textOut.getOwnerDomain.getFullName}")} & LF &
                |${scalaToVHDLString(s"${pos.fileUnixPath}:${pos.lineStart}:${pos.columnStart}")}$preambleLF""".stripMargin
          //format: on
          val args =
            if (textOut.msgArgs.isEmpty) ""
            else
              textOut.msgArgs.view.zipWithIndex.map((a, i) =>
                val argLF = if (i == textOut.msgArgs.length - 1) "" else " & LF &"
                s"${scalaToVHDLString(s"${a.get.getName} = ")} & ${csDFValToVHDLString(a)}$argLF"
              ).mkString("\n", "\n", "")
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
          else if (allParts.length == 1) scalaToVHDLString(allParts.head.asInstanceOf[String])
          else
            val unindented = allParts.lazyZip(allParts.tail :+ "").map {
              case (part: String, arg: DFVal.Ref) =>
                if (part.endsWith("\n"))
                  s"${scalaToVHDLString(part.dropRight(1))} & ${csDFValToVHDLString(arg)} & LF &\n"
                else
                  s"${scalaToVHDLString(part)} & ${csDFValToVHDLString(arg)} & "
              case (part: String, next: String) =>
                if (next.isEmpty) scalaToVHDLString(part)
                else if (part.endsWith("\n")) scalaToVHDLString(part.dropRight(1)) + " & LF &\n"
                else scalaToVHDLString(part) + " & "
              case _ => ""
            }.mkString
            if (unindented.contains("\n")) "\n" + unindented.hindent + "\n"
            else unindented
          end if
      end match
    end msg
    val reportMsg =
      if (msg.nonEmpty && !msg.contains("\n")) s" $msg " else msg
    def csReport(severity: TextOut.Severity, msg: String): String =
      s"report${reportMsg}severity ${csSeverity(severity)};"
    def csSeverity(severity: TextOut.Severity): String =
      severity match
        case TextOut.Severity.Info    => "NOTE"
        case TextOut.Severity.Warning => "WARNING"
        case TextOut.Severity.Error   => "ERROR"
        case TextOut.Severity.Fatal   => "FAILURE"
    textOut.op match
      case TextOut.Op.Finish =>
        if (inVHDL93)
          s"""report "Finished successfully (not an error)" severity ${csSeverity(
              TextOut.Severity.Fatal
            )};"""
        else
          "std.env.finish;"
      case TextOut.Op.Report(severity) => csReport(severity, msg)
      case TextOut.Op.Assert(assertionRef, severity) =>
        if (msg.isEmpty)
          s"assert ${printer.csFixedCond(assertionRef)};"
        else
          s"""|assert ${printer.csFixedCond(assertionRef)}
              |${csReport(severity, msg).hindent}""".stripMargin
      case TextOut.Op.Print => s"print($msg);"
      case TextOut.Op.Println =>
        if (msg.isEmpty) s"println(\"\");"
        else s"println($msg);"
      case TextOut.Op.Debug => csReport(TextOut.Severity.Info, msg)
    end match
  end csTextOut
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"-- $comment"
  def csDocString(doc: String): String = doc.linesIterator.mkString("--", "\n--", "")
  def csAnnotations(meta: Meta): String = ""
  // def csTimer(timer: Timer): String = unsupported
  def globalFileName: String = s"${printer.packageName}.vhd"
  def designFileName(designName: String): String = s"$designName.vhd"
  def dfhdlDefsFileName: String = s"dfhdl_pkg.vhd"
  def dfhdlSourceContents: String =
    scala.io.Source.fromResource(dfhdlDefsFileName).getLines().mkString("\n")
  override def csGlobalFileContent: String =
    // In VHDL the vectors need to be named, and put in dependency order of other named types.
    // So first we prepare the vector type declarations in a mutable map and later we remove
    // entries that were already placed in the final type printing.
    val vectorTypeDcls = mutable.Map.from(
      printer.globalVectorTypes.view.map { case (tpName, (vecType, depth)) =>
        tpName -> printer.csDFVectorDclsGlobal(DclScope.Pkg)(tpName, vecType, depth)
      }
    )
    // The body declarations can be in any order, as long as it's consistent between compilations.
    val vectorTypeDclsBody =
      printer.globalVectorTypes.view.map { case (tpName, (vecType, depth)) =>
        printer.csDFVectorDclsGlobal(DclScope.PkgBody)(tpName, vecType, depth)
      }.mkString("\n").emptyOr(x => s"$x\n")
    // collect the global named types, including vectors
    val namedDFTypes = ListSet.from(getSet.designDB.members.view.collect {
      case port @ DclPort()                     => port.dfType
      case const @ DclConst() if const.isGlobal => const.dfType
    }.flatMap(_.decompose { case dt: (DFVector | NamedDFType) => dt }))
    // declarations of the types and relevant functions
    val namedTypeConvFuncsDcl = namedDFTypes.view
      .flatMap {
        // vector types can have different dimensions, but we only need the declaration once
        case dfType: DFVector =>
          val tpName = printer.getVecDepthAndCellTypeName(dfType)._1
          vectorTypeDcls.get(tpName) match
            case Some(desc) =>
              vectorTypeDcls -= tpName
              Some(desc)
            case None => None
        case dfType: NamedDFType =>
          List(
            printer.csNamedDFTypeDcl(dfType, global = true),
            printer.csNamedDFTypeConvFuncsDcl(dfType)
          )
      }
      .mkString("\n").emptyOr(x => s"$x\n")
    val namedTypeConvFuncsBody =
      getSet.designDB.getGlobalNamedDFTypes.view
        .collect { case dfType: NamedDFType => printer.csNamedDFTypeConvFuncsBody(dfType) }
        .mkString("\n").emptyOr(x => s"$x\n")
    val usesMathReal = getSet.designDB.membersGlobals.exists {
      _.dfType.decompose { case dt: DFDouble => dt }.nonEmpty
    }
    s"""library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;${if (usesMathReal) "\nuse ieee.math_real.all;" else ""}
       |use work.dfhdl_pkg.all;
       |
       |package ${printer.packageName} is
       |${csGlobalConstIntDcls.emptyOr(_ + "\n") + namedTypeConvFuncsDcl.emptyOr(
        _ + "\n"
      ) + csGlobalConstNonIntDcls}
       |end package ${printer.packageName};
       |
       |package body ${printer.packageName} is
       |${namedTypeConvFuncsBody + vectorTypeDclsBody}
       |end package body ${printer.packageName};
       |""".stripMargin
  end csGlobalFileContent
  def alignCode(cs: String): String =
    cs
      .align(".*", ":", "[ ]*(?:in|out|inout) .*")
      .align(".*:[ ]*(?:in|out|inout)", " ", ".*")
      .align("[ ]*(?:signal|variable|constant) .*", ": ", ".*")
      .align("[ ]*[a-zA-Z0-9_.\\(\\)]+[ ]*", ":=|<=|=>", ".*")
      .align("[ ]*when [a-zA-Z0-9_.]+[ ]*", "=>", ".*")
  val vhdlKW: Set[String] = reservedKeywords
  val vhdlOps: Set[String] = Set(":=", "<=")
  val vhdlTypes: Set[String] =
    Set("std_logic", "std_logic_vector", "integer", "boolean", "natural", "positive", "ieee",
      "numeric_std", "std_logic_1164", "work", "signed", "unsigned", "'left", "string", "HT", "LF",
      "CR")
  def colorCode(cs: String): String =
    cs
      .colorWords(vhdlKW, keywordColor)
      .colorOps(vhdlOps, keywordColor)
      .colorWords(vhdlTypes, typeColor)
      .colorLineComment("--", commentColor)
end VHDLPrinter
