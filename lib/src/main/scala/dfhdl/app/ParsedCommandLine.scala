package dfhdl.app

import org.rogach.scallop.*
import dfhdl.options.{CompilerOptions, ElaborationOptions, LinterOptions, AppOptions}
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
    ao: AppOptions
) extends ScallopConf(commandArgs.toSeq):
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
    trait LintMode extends CommitMode:
      this: ScallopConf & Mode =>
      val tool = opt[LintToolSelection](
        name = "tool",
        short = 't',
        descr = "tool selection (run `help lint-tool` to get full list of linting tools)",
        default = Some(LintToolSelection(lo.verilogLinter, lo.vhdlLinter)),
        argName = "[verilogLinter][/][vhdlLinter]"
      )
      val fatalWarnings = opt[Boolean](
        descr = "warnings are fatal and produce non-zero exit code",
        default = Some(lo.fatalWarnings),
        noshort = true
      )

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
    end lint
    case object help extends Mode(DefaultMode.help, "Display usage text"):
      addSubcommand(HelpMode.backend)
      addSubcommand(HelpMode.`lint-tool`)
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

  private val programName: String =
    import dfhdl.internals.{sbtIsRunning, scala_cliIsRunning, sbtShellIsRunning}
    if (scala_cliIsRunning) s"scala run . -M $topScalaPath --"
    else if (sbtIsRunning)
      if (sbtShellIsRunning) s"runMain $topScalaPath"
      else s"""sbt "runMain $topScalaPath [options]""""
    else "<your program>"

  banner(s"Design Name: $designName\nUsage: $programName [design-args] <mode> [options]")
  appendDefaultToDescription = true
  helpFormatter = Mode.helpFormatter
  private var exitCodeOption: Option[Int] = None
  def getExitCodeOption: Option[Int] = exitCodeOption
  exitHandler = code => exitCodeOption = Some(code)
  private lazy val designArgOptionGroup = group("Design arguments:")
  private val designArgOptions =
    for (designArg <- designArgs.values)
      yield
        val conv = designArg.typeName match
          case "String"          => summon[ValueConverter[String]]
          case "Int"             => summon[ValueConverter[Int]]
          case "Double"          => summon[ValueConverter[Double]]
          case "Boolean" | "Bit" => summon[ValueConverter[Boolean]]
          case _                 => ???
        opt[Any](
          name = designArg.name, descr = designArg.desc, argName = designArg.typeName,
          default = Some(designArg.getScalaValue), noshort = true, group = designArgOptionGroup
        )(conv.asInstanceOf[ValueConverter[Any]])
  lazy val updatedDesignArgs: DesignArgs = DesignArgs(designArgs.lazyZip(designArgOptions).map {
    case ((argName, designArg), opt) =>
      (argName, designArg.updateScalaValue(opt.toOption.get))
  })

  addSubcommand(Mode.elaborate)
  addSubcommand(Mode.compile)
  addSubcommand(Mode.commit)
  addSubcommand(Mode.lint)
  addSubcommand(Mode.help)
  lazy val mode: Mode = subcommand.getOrElse(ao.defaultMode match
    case DefaultMode.help      => Mode.help
    case DefaultMode.elaborate => Mode.elaborate
    case DefaultMode.compile   => Mode.compile
    case DefaultMode.commit    => Mode.commit
    case DefaultMode.lint      => Mode.lint
  ).asInstanceOf[Mode]
  verify()
end ParsedCommandLine
