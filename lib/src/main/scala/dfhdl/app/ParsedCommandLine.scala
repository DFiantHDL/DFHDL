package dfhdl.app

import org.rogach.scallop.*
import dfhdl.options.{
  CompilerOptions,
  ElaborationOptions,
  LinterOptions,
  SimulatorOptions,
  AppOptions,
  ToolOptions,
  BuilderOptions
}
import AppOptions.DefaultMode
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
    ao: AppOptions
) extends ScallopConf(commandArgs.toSeq):
  val cacheDescYesDefault = if (ao.cacheEnable) " (default ON)" else ""
  val cacheDescNoDefault = if (!ao.cacheEnable) " (default OFF)" else ""
  val cache = toggle(
    name = "cache",
    descrYes =
      s"Enable caching$cacheDescYesDefault",
    descrNo =
      s"Disable caching$cacheDescNoDefault",
    default = Some(ao.cacheEnable),
    noshort = true
  )
  sealed abstract class Mode(val modeOption: DefaultMode, modeDesc: String)
      extends Subcommand(modeOption.toString),
        Product,
        Serializable derives CanEqual:
    if (modeOption == ao.defaultMode)
      descr(s"$modeDesc [default]")
    else
      descr(modeDesc)
    val help = opt[Boolean](hidden = true)
  object Mode:
    val helpFormatter = new ScallopHelpFormatter:
      override protected def getSubcommandHeaderPrefix = "Mode: "

    trait ElaborateMode:
      this: ScallopConf & Mode =>
      private val hidden = modeOption != DefaultMode.elaborate
      val `print-elaborate` = opt[Boolean](
        descr = "print the DFHDL design after elaboration",
        default = Some(eo.printDFHDLCode),
        noshort = true,
        hidden = hidden
      )
      val descYesDefault = if (so.Werror.toBoolean) " (default ON)" else ""
      val descNoDefault = if (!so.Werror.toBoolean) " (default OFF)" else ""
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
      private val hidden = modeOption != DefaultMode.compile
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
    end BuildMode

    case object elaborate
        extends Mode(DefaultMode.elaborate, "Elaboration only (no compilation)"),
          ElaborateMode
    case object compile
        extends Mode(
          DefaultMode.compile,
          "Compilation (after elaboration, and WITHOUT committing files to disk)"
        ),
          CompileMode:
      footer("      ~~including all elaborate command options~~")
    case object commit
        extends Mode(
          DefaultMode.commit,
          "Committing to disk (after elaboration and compilation)"
        ),
          CommitMode:
      footer("      ~~including all compile command options~~")
    case object lint
        extends Mode(
          DefaultMode.lint,
          "Linting (after elaboration, compilation, and committing to disk)"
        ),
          LintMode:
      footer("      ~~including all commit command options~~")
    case object simulate
        extends Mode(
          DefaultMode.simulate,
          "Simulating (after elaboration, compilation, and committing to disk)"
        ),
          SimulateMode:
      footer("      ~~including all commit command options~~")
    case object build
        extends Mode(
          DefaultMode.build,
          "Building (after elaboration, compilation, and committing to disk)"
        ),
          BuildMode:
      footer("      ~~including all commit command options~~")
    case object help extends Mode(DefaultMode.help, "Display usage text"):
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
  addSubcommand(Mode.help)
  lazy val mode: Mode = subcommand.getOrElse(ao.defaultMode match
    case DefaultMode.help      => Mode.help
    case DefaultMode.elaborate => Mode.elaborate
    case DefaultMode.compile   => Mode.compile
    case DefaultMode.commit    => Mode.commit
    case DefaultMode.lint      => Mode.lint
    case DefaultMode.simulate  => Mode.simulate
    case DefaultMode.build     => Mode.build).asInstanceOf[Mode]
  verify()
end ParsedCommandLine
