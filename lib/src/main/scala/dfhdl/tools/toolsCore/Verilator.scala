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

  override protected def toolFiles(using getSet: MemberGetSet): List[String] =
    getSet.designDB.srcFiles.collect {
      case SourceFile(SourceOrigin.Committed, _: VerilatorToolSource, path, _) =>
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
      ToolOptions,
      MemberGetSet
  ): String =
    val hasTiming = getSet.designDB.members.exists {
      case _: Wait => true
      case _       => false
    }
    constructCommand(
      "--lint-only",
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
      "--quiet-stats",
      s"--top-module ${topName}"
    )

  override protected[dfhdl] def lintPreprocess[D <: Design](cd: CompiledDesign[D])(using
      CompilerOptions,
      ToolOptions
  ): CompiledDesign[D] =
    addSourceFiles(
      cd,
      List(new VerilatorConfigPrinter(getInstalledVersion)(using cd.stagedDB.getSet).getSourceFile)
    )

  // override protected[dfhdl] def simulatePreprocess[D <: Design](cd: CompiledDesign[D])(using
  //     CompilerOptions,
  //     SimulatorOptions
  // ): CompiledDesign[D] =
  //   addSourceFiles(
  //     cd,
  //     List(new VerilatorSimMainPrinter(getInstalledVersion)(using cd.stagedDB.getSet).getSourceFile)
  //   )

  override protected def simulateCmdLanguageFlag(dialect: VerilogDialect): String =
    lintCmdLanguageFlag(dialect)

  override def simulate[D <: Design](
      cd: CompiledDesign[D]
  )(using CompilerOptions, SimulatorOptions): CompiledDesign[D] =
    val ret = super.simulate(cd)
    given MemberGetSet = ret.stagedDB.getSet
    val unixExec =
      s"${Paths.get(execPath).toAbsolutePath()}${separatorChar}obj_dir${separatorChar}V${topName}"
    val runExec: String =
      val osName: String = sys.props("os.name").toLowerCase
      if (osName.contains("windows")) s"${unixExec}.exe" else unixExec
    exec(cmd = "", runExec = runExec)
    ret

end Verilator

sealed trait VerilatorToolSource extends SourceType.Tool

case object VerilatorConfig extends VerilatorToolSource

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
    val fileArg =
      val sep = if (separatorChar == '\\') "\\\\" else separatorChar
      file.emptyOr(f => s""" -file "*$sep$f"""")
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

case object VerilatorSimMain extends VerilatorToolSource

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
