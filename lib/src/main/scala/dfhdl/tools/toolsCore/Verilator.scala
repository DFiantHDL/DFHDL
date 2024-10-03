package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect

object Verilator extends VerilogLinter:
  val toolName: String = "Verilator"
  protected def binExec: String = "verilator"
  override protected def windowsBinExec: String = "verilator_bin.exe"
  protected def versionCmd: String = "-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Verilator\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override val convertWindowsToLinuxPaths: Boolean = true
  protected def lintIncludeFolderFlag: String = "-I"

  override protected def toolFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(SourceOrigin.Committed, VerilatorConfig, path, _) =>
        path.convertWindowsToLinuxPaths
    }

  protected def lintCmdLanguageFlag(dialect: VerilogDialect): String =
    val language = dialect match
      case VerilogDialect.v95    => "1364-1995"
      case VerilogDialect.v2001  => "1364-2001"
      case VerilogDialect.sv2005 => "1800-2005"
      case VerilogDialect.sv2009 => "1800-2009"
      case VerilogDialect.sv2012 => "1800-2012"
      case VerilogDialect.sv2017 => "1800-2017"
    s"--default-language $language"

  override protected def lintCmdPreLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "--lint-only",
    "--quiet-stats"
  )

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      LinterOptions,
      MemberGetSet
  ): String = constructCommand(
    "-Wall"
  )

  override protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign[D] =
    addSourceFiles(
      cd,
      List(new VerilatorConfigPrinter(getInstalledVersion)(using cd.stagedDB.getSet).getSourceFile)
    )
end Verilator

case object VerilatorConfig extends SourceType.ToolConfig

class VerilatorConfigPrinter(verilatorVersion: String)(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val verilatorVersionMajor: Int = verilatorVersion.split("\\.").head.toInt
  def configFileName: String = s"${designDB.top.dclName}.vlt"
  def contents: String =
    s"""`verilator_config
       |$commands
       |""".stripMargin
  def commands: String =
    lintOffHidden.emptyOr(_ + "\n") +
      lintOffBlackBoxes.emptyOr(_ + "\n") +
      lintOffOpenOutPorts.emptyOr(_ + "\n") +
      lintOffUnused.emptyOr(_ + "\n") +
      lintOffUnusedBits.emptyOr(_ + "\n") +
      lintOffUnusedParam.emptyOr(_ + "\n") +
      lintOffWidthExpand.emptyOr(_ + "\n")
  def lintOffCommand(
      rule: String = "",
      file: String = "",
      lines: String = "",
      matchWild: String = ""
  ): String =
    val ruleArg = rule.emptyOr(" -rule " + _)
    val fileArg = file.emptyOr(f => s""" -file "*$separatorChar$f"""")
    val lineArg = lines.emptyOr(" -lines " + _)
    val matchWildArg = matchWild.emptyOr(m => s""" -match "$m"""")
    s"lint_off$ruleArg$fileArg$lineArg$matchWildArg"
  def lintOffHidden: String = lintOffCommand("VARHIDDEN")
  def lintOffBlackBoxes: String =
    designDB.srcFiles.flatMap {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.BlackBox, path, _) =>
        val fileNameStr = Paths.get(path).getFileName.toString
        List(
          lintOffCommand(rule = "UNUSEDSIGNAL", file = fileNameStr),
          lintOffCommand(rule = "UNDRIVEN", file = fileNameStr)
        )
      case _ => None
    }.mkString("\n")
  end lintOffBlackBoxes
  def lintOffOpenOutPorts: String =
    designDB.getOpenOutPorts.map: dfVal =>
      lintOffCommand(
        rule = "PINCONNECTEMPTY",
        file = s"${dfVal.getOwnerDesign.getOwnerDesign.dclName}.*",
        matchWild = s"*: '${dfVal.getName}'*"
      )
    .distinct.mkString("\n")
  def lintOffUnused: String =
    designDB.getUnusedAnnotValues.map: dfVal =>
      lintOffCommand(
        rule = "UNUSEDSIGNAL",
        file = s"${dfVal.getOwnerDesign.dclName}.*",
        matchWild = s"*: '${dfVal.getName}'*"
      )
    .distinct.mkString("\n")
  def lintOffUnusedBits: String =
    designDB.getUnusedBitsValues.map: (dfVal, relBitHigh, relBitLow) =>
      val bitSel =
        if (relBitHigh == relBitLow) s"$relBitHigh"
        else s"$relBitHigh:$relBitLow"
      lintOffCommand(
        rule = "UNUSEDSIGNAL",
        file = s"${dfVal.getOwnerDesign.dclName}.*",
        matchWild = s"*Bits of signal are not used: '${dfVal.getName}'[$bitSel]*"
      )
    .distinct.mkString("\n")
  def lintOffUnusedParam: String =
    if (verilatorVersionMajor >= 5) // only supported from version 5 onwards
      designDB.getUnusedParamAnnotValues.map: dfVal =>
        lintOffCommand(
          rule = "UNUSEDPARAM",
          file = s"${dfVal.getOwnerDesign.dclName}.*",
          matchWild = s"*: '${dfVal.getName}'*"
        )
      .distinct.mkString("\n")
    else ""
  def lintOffWidthExpand: String =
    co.backend.asInstanceOf[backends.verilog].dialect match
      // only relevant for non-SystemVerilog dialects
      case VerilogDialect.v95 | VerilogDialect.v2001 =>
        lintOffCommand(
          rule = "WIDTHEXPAND",
          file = s"*.*",
          matchWild = s"*expects 32 bits*"
        )
      case _ => ""
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VerilatorConfig, configFileName, contents)

end VerilatorConfigPrinter
