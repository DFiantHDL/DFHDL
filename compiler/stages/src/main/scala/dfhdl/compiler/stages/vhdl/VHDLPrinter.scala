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
  def csStep(step: Step): String = unsupported
  def csGoto(goto: Goto): String = unsupported
  def csDFRange(range: DFRange): String = unsupported
  def csWait(wait: Wait): String =
    val trigger = wait.triggerRef.get
    trigger.dfType match
      case _: DFBoolOrBit =>
        trigger match
          // rising or falling edge does not need to be negated
          case DFVal.Func(_, FuncOp.rising | FuncOp.falling, _, _, _, _) =>
            s"wait until ${wait.triggerRef.refCodeString};"
          // no need for `not not`, so just skipping the not operation
          case DFVal.Func(_, FuncOp.unary_!, List(triggerRef), _, _, _) =>
            s"wait until ${triggerRef.refCodeString};"
          case _ =>
            s"wait until not ${wait.triggerRef.refCodeString};"
      case DFTime | DFCycles => s"wait for ${wait.triggerRef.refCodeString};"
      case _                 => ???
  end csWait
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
    Set("std_logic", "std_logic_vector", "integer", "natural", "positive", "ieee", "numeric_std",
      "std_logic_1164", "work", "signed", "unsigned", "'left")
  def colorCode(cs: String): String =
    cs
      .colorWords(vhdlKW, keywordColor)
      .colorOps(vhdlOps, keywordColor)
      .colorWords(vhdlTypes, typeColor)
      .colorLineComment("--", commentColor)
end VHDLPrinter
