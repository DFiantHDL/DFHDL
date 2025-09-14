package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.backends
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, ToolOptions, LinterOptions, SimulatorOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import java.io.FileWriter
import java.io.File.separatorChar
import dfhdl.compiler.stages.verilog.VerilogDialect

object Verilator extends VerilogLinter, VerilogSimulator:
  val toolName: String = "Verilator"
  protected def binExec: String = "verilator"
  override protected def windowsBinExec: String = "verilator_bin.exe"
  protected def versionCmd: String = "-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """Verilator\s+(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override val convertWindowsToLinuxPaths: Boolean = true
  protected def includeFolderFlag: String = "-I"

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
      ToolOptions,
      MemberGetSet
  ): String =
    val hasTiming = getSet.designDB.members.exists {
      case _: Wait => true
      case _       => false
    }
    constructCommand(
      "--lint-only",
      "--assert",
      "--quiet-stats",
      s"--top-module ${topName}",
      if (hasTiming) "--timing" else ""
    )
  end lintCmdPreLangFlags

  override protected def lintCmdPostLangFlags(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): String = constructCommand(
    "-Wall",
    (!summon[LinterOptions].Werror.toBoolean).toFlag("-Wno-fatal")
  )

  override protected def simulateCmdPreLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String =
    constructCommand(
      "--binary",
      "--assert",
      "--quiet-stats",
      "--build-jobs 0",
      s"--top-module ${topName}"
    )

  override protected def simulateCmdPostLangFlags(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): String = constructCommand(
    "-Wall",
    (!summon[LinterOptions].Werror.toBoolean).toFlag("-Wno-fatal")
  )

  override protected[dfhdl] def lintPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(new VerilatorConfigPrinter(getInstalledVersion)(using cd.stagedDB.getSet).getSourceFile)
    )

  override protected def lintLogger(using
      CompilerOptions,
      ToolOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => line.startsWith("%Warning"),
        lineIsSuppressed = (line: String) => false
      )
    )

  override protected[dfhdl] def simulatePreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      SimulatorOptions
  ): CompiledDesign =
    val linted = lintPreprocess(cd)
    given MemberGetSet = linted.stagedDB.getSet
    exec(simulateCmdFlags, (), simulateLogger, simRunExec)
    linted

  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    lintCmdLanguageFlag(dialect)

  override protected def simulateLogger(using
      CompilerOptions,
      SimulatorOptions,
      MemberGetSet
  ): Option[Tool.ProcessLogger] =
    var totalCompilations = 0
    var cpps = 0
    var silence = false
    def setCppsFromFolder(): Unit =
      val objDirPath = s"${execPath}${separatorChar}obj_dir"
      val objDir = new java.io.File(objDirPath)
      if (objDir.exists && objDir.isDirectory)
        val cppFiles = objDir.listFiles.count(f => f.isFile && f.getName.endsWith(".cpp"))
        totalCompilations = cppFiles + 1 // +1 for final linking
    // Create a process logger to silence the detailed make output and show progress
    Some(
      Tool.ProcessLogger(
        lineIsWarning = (line: String) => false,
        lineIsSuppressed = (line: String) =>
          val ret =
            if (line.startsWith("make: Entering directory"))
              setCppsFromFolder()
              silence = true
              true
            else if (line.startsWith("g++"))
              cpps += 1
              true
            else if (line.startsWith("make: Leaving directory"))
              cpps = totalCompilations
              silence = false
              true
            else if (line.endsWith("verilator_deplist.tmp")) true
            else silence
          // Print progress percentage
          if (totalCompilations > 0)
            val percentage = (cpps * 100) / totalCompilations
            print(s"\rCompiling verilated C++ files: $percentage%")
            if (cpps >= totalCompilations)
              println() // Add a newline when complete
              totalCompilations = 0
              cpps = 0
          ret
      )
    )
  end simulateLogger

  def verilatedBinary(using MemberGetSet): String =
    val bin = s"obj_dir${separatorChar}V${topName}"
    if (osIsWindows) s"${bin}.exe" else bin

  override protected[dfhdl] def producedFiles(using
      MemberGetSet,
      CompilerOptions,
      SimulatorOptions
  ): List[String] =
    List(verilatedBinary)

  override def simulate(
      cd: CompiledDesign
  )(using co: CompilerOptions, so: SimulatorOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    // set special logger to identify warnings, because verilator does not track warnings.
    val logger = Some(
      Tool.ProcessLogger(
        // `WARNING:` is used by DFHDL when compiling to v95/v2001 dialects
        lineIsWarning = (line: String) => line.startsWith("WARNING:") || line.contains("%Warning:"),
        lineIsSuppressed = (line: String) =>
          // the user does not need to see this
          line.endsWith("ignored due to +verilator+error+limit")
      )
    )
    val cmd = constructCommand(
      "+verilator+quiet",
      s"+verilator+error+limit+${Int.MaxValue}"
    )
    exec(cmd = cmd, loggerOpt = logger, runExec = verilatedBinary)
    cd
  end simulate

end Verilator

val VerilatorConfig = SourceType.Tool("Verilator", "Config")

class VerilatorConfigPrinter(verilatorVersion: String)(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val verilatorVersionMajor: Int = verilatorVersion.split("\\.").head.toInt
  def configFileName: String = s"${getSet.topName}.vlt"
  def contents: String =
    s"""`verilator_config
       |$commands
       |""".stripMargin
  def commands: String =
    lintOffNoParamDefaults.emptyOr(_ + "\n") +
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
    val fileArg =
      val sep = if (separatorChar == '\\') "\\\\" else separatorChar
      file.emptyOr(f => s""" -file "*$sep$f"""")
    val lineArg = lines.emptyOr(" -lines " + _)
    val matchWildArg = matchWild.emptyOr(m => s""" -match "$m"""")
    s"lint_off$ruleArg$fileArg$lineArg$matchWildArg"
  def lintOffHidden: String = lintOffCommand("VARHIDDEN")
  def lintOffNoParamDefaults: String =
    if (co.backend.asInstanceOf[backends.verilog].dialect == VerilogDialect.sv2005)
      lintOffCommand("NEWERSTD", file = "*.*", matchWild = "*Parameter requires default value*")
    else ""
  def lintOffBlackBoxes: String =
    designDB.srcFiles.flatMap {
      case SourceFile(SourceOrigin.Committed, SourceType.BlackBox, path, _) =>
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
    designDB.getUnusedBitsValues.map: (dfVal, idxHigh, idxLow) =>
      val bitSel =
        if (idxHigh == idxLow) s"$idxHigh"
        else s"$idxHigh:$idxLow"
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

// val VerilatorSimMain = SourceType.Tool("Verilator", "SimMain")

// class VerilatorSimMainPrinter(verilatorVersion: String)(using
//     getSet: MemberGetSet,
//     co: CompilerOptions,
//     so: SimulatorOptions
// ):
//   val designDB: DB = getSet.designDB
//   val topName = designDB.top.dclName
//   def mainFileName: String = s"${topName}.cpp"
//   def contents: String =
//     s"""|#include "V${topName}.h"
//         |#include "verilated.h"
//         |#include "V${topName}___024root.h"
//         |
//         |int main(int argc, char** argv) {
//         |    // Initialize Verilator
//         |    Verilated::commandArgs(argc, argv);
//         |
//         |    // Create instance of our module
//         |    V${topName}* top = new V${topName};
//         |
//         |    // Initialize simulation inputs
//         |    V${topName}___024root* rootp = top->rootp;
//         |    rootp->${topName}__DOT__rst = 1;
//         |
//         |    while (!Verilated::gotFinish()) {
//         |        // Toggle clock
//         |        rootp->${topName}__DOT__clk = 0;
//         |        top->eval();
//         |        rootp->${topName}__DOT__clk = 1;
//         |        top->eval();
//         |        rootp->${topName}__DOT__rst = 0;
//         |    }
//         |
//         |    // Cleanup
//         |    delete top;
//         |
//         |    return 0;
//         |}
//         |""".stripMargin
//   def getSourceFile: SourceFile =
//     SourceFile(SourceOrigin.Compiled, VerilatorSimMain, mainFileName, contents)
// end VerilatorSimMainPrinter
