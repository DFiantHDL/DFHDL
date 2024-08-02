package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.options.VerilatorOptions

object Verilator extends VerilogLinter:
  type LO = VerilatorOptions
  val toolName: String = "Verilator"
  def binExec: String =
    val osName: String = sys.props("os.name").toLowerCase
    if (osName.contains("windows")) "verilator_bin" else "verilator"

  def commonFlags(using lo: LO): String = s"-Wall${lo.warnAsError.toFlag("--Werror")} "
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

    val globalIncludeFolder = cd.stagedDB.srcFiles.collectFirst {
      case SourceFile(SourceOrigin.Committed, SourceType.Design.GlobalDef, path, _) =>
        Paths.get(path).getParent.toString.forceWindowsToLinuxPath
    }.get

    // config files must be placed before the design sources
    s"-I$globalIncludeFolder $configsInCmd $designsInCmd"
  end filesCmdPart
  override protected[dfhdl] def preprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions
  ): CompiledDesign[D] =
    addSourceFiles(cd, List(new VerilatorConfigPrinter(using cd.stagedDB.getSet).getSourceFile))
  def lint[D <: Design](cd: CompiledDesign[D])(using CompilerOptions, LO): CompiledDesign[D] =
    exec(
      cd,
      s"$binExec --lint-only $commonFlags ${filesCmdPart(cd)}"
    )
  end lint
end Verilator

case object VerilatorConfig extends SourceType.ToolConfig

class VerilatorConfigPrinter(using getSet: MemberGetSet):
  val designDB: DB = getSet.designDB
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
      lintOffUnusedBits.emptyOr(_ + "\n")
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
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VerilatorConfig, configFileName, contents)

end VerilatorConfigPrinter
