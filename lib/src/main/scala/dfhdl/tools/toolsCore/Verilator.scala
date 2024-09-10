package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, VerilatorOptions, ToolOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect

object Verilator extends VerilogLinter:
  type LO = VerilatorOptions
  val toolName: String = "Verilator"
  protected def binExec: String = "verilator"
  override protected def windowsBinExec: String = "verilator_bin.exe"
  protected def versionCmd: String = "-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Verilator\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  def commonFlags(using co: CompilerOptions, lo: LO): String =
    val language = co.backend match
      case be: backends.verilog =>
        be.dialect match
          case VerilogDialect.v2001 => "1364-2001"
          case VerilogDialect.sv2005 => "1800-2005"
          case VerilogDialect.sv2009 => "1800-2009"
          case VerilogDialect.sv2012 => "1800-2012"
          case VerilogDialect.sv2017 => "1800-2017"
      case _ =>
        throw new java.lang.IllegalArgumentException(
          "Current backend is not supported for Verilator linting."
        )
    s"-Wall${lo.warnAsError.toFlag("--Werror")} --default-language $language "
  def filesCmdPart[D <: Design](cd: CompiledDesign[D]): String =
    // We use `forceWindowsToLinuxPath` fit the verilator needs
    val designsInCmd = cd.stagedDB.srcFiles.view.collect {
      case SourceFile(
            SourceOrigin.Committed,
            SourceType.Design.Regular | SourceType.Design.BlackBox,
            path,
            _
          ) =>
        path.forceWindowsToLinuxPath
    }.mkString(" ")

    val configsInCmd = cd.stagedDB.srcFiles.view.collect {
      case SourceFile(SourceOrigin.Committed, VerilatorConfig, path, _) =>
        path.forceWindowsToLinuxPath
    }.mkString(" ")

    val dfhdlDefsIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.DFHDLDef, path, _) =>
        Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    val globalIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    val includes =
      List(dfhdlDefsIncludeFolder, globalIncludeFolder).distinct.map(i => s"-I$i").mkString(" ")

    // config files must be placed before the design sources
    s"$includes $configsInCmd $designsInCmd"
  end filesCmdPart
  override protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign[D] =
    addSourceFiles(
      cd,
      List(new VerilatorConfigPrinter(getInstalledVersion)(using cd.stagedDB.getSet).getSourceFile)
    )
  def lint[D <: Design](cd: CompiledDesign[D])(using CompilerOptions, LO): CompiledDesign[D] =
    exec(
      cd,
      s"--lint-only $commonFlags ${filesCmdPart(cd)}"
    )
  end lint
end Verilator

case object VerilatorConfig extends SourceType.ToolConfig

class VerilatorConfigPrinter(verilatorVersion: String)(using getSet: MemberGetSet):
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
      lintOffUnusedParam.emptyOr(_ + "\n")
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
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VerilatorConfig, configFileName, contents)

end VerilatorConfigPrinter
