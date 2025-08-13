package dfhdl.app

import org.rogach.scallop.*
import dfhdl.options.*
import dfhdl.internals.scastieIsRunning
import dfhdl.internals.sbtShellIsRunning

class ParsedCommandLine(
    designName: String,
    topScalaPath: String,
    designArgs: DesignArgs,
    commandArgs: Array[String]
)(using
    eo: ElaborationOptions,
    co: CompilerOptions,
    lo: LinterOptions,
    so: SimulatorOptions,
    bo: BuilderOptions,
    po: ProgrammerOptions,
    ao: AppOptions
) extends ScallopConf(commandArgs.toSeq):
  val cache = toggle(
    name = "cache",
    descrYes = "Enable caching",
    descrNo = "Disable caching",
    default = Some(ao.cacheEnable),
    noshort = true
  )
  sealed abstract class Mode(val modeOption: AppMode, modeDesc: String)
      extends Subcommand(modeOption.toString),
        Product,
        Serializable derives CanEqual:
    if (modeOption == ao.appMode)
      descr(s"$modeDesc [default]")
    else
      descr(modeDesc)
    val help = opt[Boolean](hidden = true)
  object Mode:
    val helpFormatter = new ScallopHelpFormatter:
      override protected def getSubcommandHeaderPrefix = "Mode: "

    trait ElaborateMode:
      this: ScallopConf & Mode =>
      private val hidden = modeOption != AppMode.elaborate
      val `print-elaborate` = opt[Boolean](
        descr = "print the DFHDL design after elaboration",
        default = Some(eo.printDFHDLCode),
        noshort = true,
        hidden = hidden
      )
      private val descYesDefault = if (eo.Werror.toBoolean) " (default)" else ""
      private val descNoDefault = if (!eo.Werror.toBoolean) " (default)" else ""
      val Werror = toggle(
        name = "Werror",
        descrYes =
          s"Turn on: elaboration warnings are fatal and produce non-zero exit code$descYesDefault",
        descrNo =
          s"Turn off: elaboration warnings are not fatal and produce zero exit code$descNoDefault",
        default = Some(eo.Werror.toBoolean),
        noshort = true,
        hidden = hidden
      )
    end ElaborateMode
    trait CompileMode extends ElaborateMode:
      this: ScallopConf & Mode =>
      private val hidden = modeOption != AppMode.compile
      val backend = opt[CompilerOptions.Backend](
        name = "backend", short = 'b',
        descr = "backend selection (run `help backend` to get full list of languages and dialects)",
        default = Some(co.backend), argName = "lang[.dialect]", hidden = hidden
      )
      val `print-compile` = opt[Boolean](
        descr = "print the DFHDL design after compilation",
        default = Some(co.printDFHDLCode),
        hidden = hidden,
        noshort = true
      )
      val `print-backend` = opt[Boolean](
        descr = "print the backend design after compilation",
        default = Some(co.printBackendCode),
        hidden = hidden,
        noshort = true
      )
    end CompileMode
    trait CommitMode extends CompileMode:
      this: ScallopConf & Mode =>
    trait ToolMode extends CommitMode:
      this: ScallopConf & Mode =>
      lazy val options: ToolOptions
      private lazy val descYesDefault = if (options.Werror.toBoolean) " (default)" else ""
      private lazy val descNoDefault = if (!options.Werror.toBoolean) " (default)" else ""
      val `Werror-tool` = toggle(
        name = "Werror-tool",
        descrYes =
          s"Turn on: tool warnings are fatal and produce non-zero exit code$descYesDefault",
        descrNo = s"Turn off: tool warnings are not fatal and produce zero exit code$descNoDefault",
        default = Some(options.Werror.toBoolean),
        noshort = true
      )
    trait LintMode extends ToolMode:
      this: ScallopConf & Mode =>
      lazy val options = lo
      val tool = opt[LintToolSelection](
        name = "tool",
        short = 't',
        descr = "tool selection (run `help lint-tool` to get full list of linting tools)",
        default = Some(LintToolSelection(lo.verilogLinter, lo.vhdlLinter)),
        argName = "[verilogLinter][/][vhdlLinter]"
      )
    end LintMode
    trait SimulateMode extends ToolMode:
      this: ScallopConf & Mode =>
      lazy val options = so
      val tool = opt[SimulateToolSelection](
        name = "tool",
        short = 't',
        descr = "tool selection (run `help simulate-tool` to get full list of simulation tools)",
        default = Some(SimulateToolSelection(so.verilogSimulator, so.vhdlSimulator)),
        argName = "[verilogSimulator][/][vhdlSimulator]"
      )
    end SimulateMode
    trait BuildMode extends ToolMode:
      this: ScallopConf & Mode =>
      lazy val options = bo
      private val descYesDefault = if (bo.flash) " (default)" else ""
      private val descNoDefault = if (!bo.flash) " (default)" else ""
      val tool = choice(
        choices = Seq("foss", "vendor"),
        default = Some(bo.tool.toString),
        name = "tool",
        short = 't',
        descr = "tool selection: `foss` for free and open source tools, `vendor` for vendor tools."
      )
      val flash = toggle(
        name = "flash",
        descrYes = s"Create also a flash image for an on-board flash device$descYesDefault",
        descrNo = s"Create only a bitstream file to program the FPGA$descNoDefault",
        default = Some(bo.flash),
        noshort = true
      )
    end BuildMode
    trait ProgramMode extends ToolMode:
      this: ScallopConf & Mode =>
      lazy val options = po
      private val descYesDefault = if (po.flash) " (default)" else ""
      private val descNoDefault = if (!po.flash) " (default)" else ""
      val tool = choice(
        choices = Seq("foss", "vendor"),
        default = Some(po.tool.toString),
        name = "tool",
        short = 't',
        descr = "tool selection: `foss` for free and open source tools, `vendor` for vendor tools."
      )
      val flash = toggle(
        name = "flash",
        descrYes = s"Program the on-board flash device$descYesDefault",
        descrNo = s"Program the FPGA only$descNoDefault",
        default = Some(po.flash),
        noshort = true
      )
    end ProgramMode

    case object elaborate
        extends Mode(AppMode.elaborate, "Elaboration only (no compilation)"),
          ElaborateMode
    case object compile
        extends Mode(
          AppMode.compile,
          "Compilation (after elaboration, and WITHOUT committing files to disk)"
        ),
          CompileMode:
      footer("      ~~including all elaborate command options~~")
    case object commit
        extends Mode(
          AppMode.commit,
          "Committing to disk (after elaboration and compilation)"
        ),
          CommitMode:
      footer("      ~~including all compile command options~~")
    case object lint
        extends Mode(
          AppMode.lint,
          "Linting (after elaboration, compilation, and committing to disk)"
        ),
          LintMode:
      footer("      ~~including all commit command options~~")
    case object simulate
        extends Mode(
          AppMode.simulate,
          "Simulating (after elaboration, compilation, and committing to disk)"
        ),
          SimulateMode:
      footer("      ~~including all commit command options~~")
    case object build
        extends Mode(
          AppMode.build,
          "Building (after elaboration, compilation, and committing to disk)"
        ),
          BuildMode:
      footer("      ~~including all commit command options~~")
    case object program
        extends Mode(
          AppMode.program,
          "Programming (after elaboration, compilation, committing to disk, and building)"
        ),
          ProgramMode:
      footer("      ~~including all build command options~~")
    case object help extends Mode(AppMode.help, "Display usage text"):
      addSubcommand(HelpMode.backend)
      addSubcommand(HelpMode.`lint-tool`)
      addSubcommand(HelpMode.`simulate-tool`)
  end Mode

  sealed abstract class HelpMode(cmdName: String) extends Subcommand(cmdName), Product, Serializable
      derives CanEqual:
    val help = opt[Boolean](hidden = true)
  object HelpMode:
    case object backend extends HelpMode("backend"):
      descr("List all backend languages and dialects")
    case object `lint-tool` extends HelpMode("lint-tool"):
      descr("List all integrated linting tools")
      val scan = opt[Boolean](
        descr = "scan the system path for available tools to run",
        default = Some(false)
      )
    case object `simulate-tool` extends HelpMode("simulate-tool"):
      descr("List all integrated simulation tools")
      val scan = opt[Boolean](
        descr = "scan the system path for available tools to run",
        default = Some(false)
      )
  end HelpMode

  private def usageText(options: String): String =
    import dfhdl.internals.{sbtIsRunning, scala_cliIsRunning, sbtShellIsRunning}
    if (scala_cliIsRunning) s"scala run . -M $topScalaPath -- $options"
    else if (sbtIsRunning)
      if (sbtShellIsRunning && !scastieIsRunning) s"runMain $topScalaPath $options"
      else s"""sbt "runMain $topScalaPath $options""""
    else s"<your program> $options"

  banner(s"""Design Name: $designName\nUsage: ${usageText("[design-args] <mode> [options]")} """)
  appendDefaultToDescription = true
  helpFormatter = Mode.helpFormatter
  private var exitCodeOption: Option[Int] = None
  def getExitCodeOption: Option[Int] = exitCodeOption
  exitHandler = code => exitCodeOption = Some(code)
  private lazy val designArgOptionGroup = group("Design arguments:")
  private val designArgOptions =
    designArgs.view.values.collect {
      case designArg if designArg.typeName.nonEmpty =>
        val conv = designArg.typeName match
          case "String"  => summon[ValueConverter[String]]
          case "Int"     => summon[ValueConverter[Int]]
          case "Double"  => summon[ValueConverter[Double]]
          case "Boolean" => summon[ValueConverter[Boolean]]
        designArg.name -> opt[Any](
          name = designArg.name, descr = designArg.desc, argName = designArg.typeName,
          default = Some(designArg.getScalaValue), noshort = true, group = designArgOptionGroup
        )(using conv.asInstanceOf[ValueConverter[Any]])
    }.toMap
  lazy val updatedDesignArgs: DesignArgs = DesignArgs(designArgs.map { case (argName, designArg) =>
    designArgOptions.get(argName) match
      case None      => argName -> designArg
      case Some(opt) => argName -> designArg.updateScalaValue(opt.toOption.get)
  })

  addSubcommand(Mode.elaborate)
  addSubcommand(Mode.compile)
  addSubcommand(Mode.commit)
  addSubcommand(Mode.lint)
  addSubcommand(Mode.simulate)
  addSubcommand(Mode.build)
  addSubcommand(Mode.program)
  addSubcommand(Mode.help)
  lazy val mode: Mode = subcommand.getOrElse(ao.appMode match
    case AppMode.help      => Mode.help
    case AppMode.elaborate => Mode.elaborate
    case AppMode.compile   => Mode.compile
    case AppMode.commit    => Mode.commit
    case AppMode.lint      => Mode.lint
    case AppMode.simulate  => Mode.simulate
    case AppMode.build     => Mode.build
    case AppMode.program   => Mode.program).asInstanceOf[Mode]
  verify()
end ParsedCommandLine
