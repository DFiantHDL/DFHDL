package dfhdl
import scala.collection.immutable.ListMap
import dfhdl.core
import core.{DFValAny, asValAny}
import dfhdl.compiler.ir
import wvlet.log.{Logger, LogFormatter}
import scala.collection.mutable
import scala.annotation.static
import dfhdl.options.CompilerOptions
trait DFApp:
  private val logger = Logger("DFHDL App")
  logger.setFormatter(LogFormatter.BareFormatter)
  logger.info(s"Welcome to DFiant HDL (DFHDL) v$dfhdlVersion !!!")
  private var designName: String = ""
  private var topScalaPath: String = ""
  // this context is just for enabling `getConstData` to work.
  // the internal global context inside `value` will be actually at play here.
  val dfc: DFC = DFC.emptyNoEO
  case class Arg(name: String, typeName: String, value: Any, desc: String):
    def valueStr: String = value match
      case dfConst: DFValAny =>
        import dfc.getSet
        dfConst.asIR.getConstData.asInstanceOf[Option[Option[Any]]].get.get.toString()
      case _ => value.toString()
    def updateWithValueStr(updatedValueStr: String): Arg =
      val scalaTypeName = typeName.replaceFirst("DFHDL ", "")
      val updatedValueScala = scalaTypeName match
        case "Int"    => BigInt(updatedValueStr)
        case "Double" => updatedValueStr.toDouble
        case "Boolean" | "Bit" =>
          updatedValueStr match
            case "1" => true
            case "0" => false
            case _   => updatedValueStr.toBoolean
        case _ => updatedValueStr

      val updatedValue = value match
        case dfConst: DFValAny =>
          core.DFVal.Const.forced(dfConst.dfType, Some(updatedValueScala))
        case _ => updatedValueScala
      copy(value = updatedValue)
    end updateWithValueStr
  end Arg

  private var designArgs: ListMap[String, Arg] = ListMap.empty
  private var elaborationOptions: options.ElaborationOptions = null
  private var compilerOptions: options.CompilerOptions = null
  private var printerOptions: options.PrinterOptions = null
  private var linterOptions: options.LinterOptions = null
  given options.CompilerOptions = compilerOptions
  given options.PrinterOptions = printerOptions
  private var dsn: () => core.Design = null
  private var mode: String = "commit"
  private var helpMode: String = "usage"
  // used by the plugin to get the updated design arguments that could be changed by the
  // command-line options
  final protected def getDsnArg(name: String): Any =
    designArgs(name).value
  // used by the plugin to get the updated elaboration options that could be changed by the
  // command-line options
  final protected def getElaborationOptions: options.ElaborationOptions = elaborationOptions
  final protected def setInitials(
      _designName: String,
      _topScalaPath: String,
      top: dfhdl.top,
      argNames: List[String],
      argTypes: List[String],
      argValues: List[Any],
      argDescs: List[String]
  ): Unit =
    designName = _designName
    topScalaPath = _topScalaPath
    elaborationOptions = top.elaborationOptions
    compilerOptions = top.compilerOptions
    printerOptions = top.printerOptions
    designArgs = ListMap.from(
      argNames.lazyZip(argTypes).lazyZip(argValues).lazyZip(argDescs).map(
        (name, typeName, value, desc) => name -> Arg(name, typeName, value, desc)
      )
    )
  end setInitials
  final protected def setDsn(d: => core.Design): Unit = dsn = () => d
  private def elaborate: core.Design =
    logger.info("Elaborating design...")
    // the elaboration options are set in the compiler plugin using getElaborationOptions
    val elaborated = dsn()
    if (elaborationOptions.printDesignCodeAfter)
      println(
        """|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |The design code after elaboration:
           |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~""".stripMargin
      )
      elaborated.printCodeString
    elaborated

  private def programName: String =
    import dfhdl.internals.{sbtIsRunning, scala_cliIsRunning, sbtShellIsRunning}
    if (scala_cliIsRunning) s"scala-cli run . -M $topScalaPath --"
    else if (sbtIsRunning)
      if (sbtShellIsRunning) s"runMain $topScalaPath"
      else s"""sbt "runMain $topScalaPath [options]""""
    else "<your program>"
  private lazy val parser = new scopt.OptionParser[Unit](programName):
    override def terminate(exitState: Either[String, Unit]): Unit = ()
    def optDesignArg = opt[Map[String, String]]("design-args")
      .abbr("da")
      .valueName("a1=v1,a2=v2...")
      .validate(x =>
        val invalidKeys = x.keySet -- designArgs.keySet
        if (invalidKeys.nonEmpty)
          failure(s"Unrecognized design arguments: ${invalidKeys.mkString(", ")}")
        else
          val typeMismatches = x.flatMap((argName, updatedValueStr) =>
            val isValid =
              designArgs(argName).typeName match
                case "String" | "DFHDL String" => true
                case "Int" | "DFHDL Int"       => updatedValueStr.toIntOption.nonEmpty
                case "Double" | "DFHDL Double" => updatedValueStr.toDoubleOption.nonEmpty
                case "Boolean" | "Bit" | "DFHDL Boolean" | "DFHDL Bit" =>
                  updatedValueStr.toBooleanOption
                    .nonEmpty || updatedValueStr == "1" || updatedValueStr == "0"
                case _ => false
            if (isValid) None
            else Some(argName)
          )
          if (typeMismatches.nonEmpty)
            failure(s"Type mismatch in design arguments: ${typeMismatches.mkString(", ")}")
          else success
        end if
      )
      .foreach(_.foreach { (argName, updatedValueStr) =>
        designArgs =
          designArgs.updatedWith(argName)(ao => Some(ao.get.updateWithValueStr(updatedValueStr)))
      })
      .text("design arguments (run list-design-args command to get a full list)")
    def optLinter =
      val accepted = Set("verilator", "ghdl")
      opt[String]("linter")
        .validate(x =>
          if (accepted.contains(x)) success
          else failure(s"Acceptable values are: ${accepted.mkString(", ")}")
        )
        .text(s"linter tool: ${accepted.mkString(", ")}")
    head(s"Design Name: $designName")
    optDesignArg
    def backendOption =
      val defaultValue = compilerOptions.backend match
        case backend: dfhdl.backends.verilog => s"verilog:${backend.dialect}"
        case backend: dfhdl.backends.vhdl    => s"vhdl:${backend.dialect}"
      def parseValue(value: String): Either[String, CompilerOptions.Backend] =
        value.split(":").toList match
          case lang :: Nil =>
            lang match
              case "verilog" => Right(dfhdl.backends.verilog)
              case "vhdl"    => Right(dfhdl.backends.vhdl)
              case _         => Left(s"Invalid backend language: $lang")
          case lang :: dialect :: Nil =>
            lang match
              case "verilog" =>
                dialect match
                  case "v2001"  => Right(dfhdl.backends.verilog.v2001)
                  case "sv2005" => Right(dfhdl.backends.verilog.sv2005)
                  case "sv2012" => Right(dfhdl.backends.verilog.sv2012)
                  case "sv2017" => Right(dfhdl.backends.verilog.sv2017)
                  case _        => Left(s"Invalid Verilog/SystemVerilog backend dialect: $dialect")
              case "vhdl" =>
                dialect match
                  case "v93"   => Right(dfhdl.backends.vhdl.v93)
                  case "v2008" => Right(dfhdl.backends.vhdl.v2008)
                  case "v2019" => Right(dfhdl.backends.vhdl.v2019)
                  case _       => Left(s"Invalid VHDL backend dialect: $dialect")
              case _ => Left(s"Invalid backend language: $lang")
          case _ => Left("Invalid backend syntax. Found too many separator colons.")
      opt[String]("backend")
        .abbr("b")
        .validate(x => parseValue(x).map(_ => ()))
        .foreach(x => compilerOptions = compilerOptions.copy(backend = parseValue(x).toOption.get))
        .valueName(s"<lang>[:<dialect>]  default=$defaultValue")
        .text("backend selection (run `help backend` to get full list of languages and dialects)")
    end backendOption
    backendOption
    opt[Unit]("print-elaborate")
      .abbr("pe")
      .foreach(_ => elaborationOptions = elaborationOptions.copy(printDesignCodeAfter = true))
      .text("print the DFHDL design after elaboration")
    opt[Unit]("print-compile")
      .abbr("pc")
      .foreach(_ => compilerOptions = compilerOptions.copy(printDesignCodeAfter = true))
      .text("print the DFHDL design after compilation")
    opt[Unit]("print-backend")
      .abbr("pb")
      .foreach(_ => compilerOptions = compilerOptions.copy(printGenFiles = true))
      .text("print the backend design after compilation")
    cmd("help")
      .foreach(_ => mode = "help")
      .text("Display usage text")
      .children(
        cmd("backend")
          .foreach(_ => helpMode = "backend")
          .text("List all backend languages and dialects"),
        cmd("design-args")
          .foreach(_ => helpMode = "design-args")
          .text("List all design arguments")
      )
    cmd("elaborate")
      .foreach(_ => mode = "elaborate")
      .text("Mode: Elaboration only (no compilation)")
    cmd("compile")
      .foreach(_ => mode = "compile")
      .text("Mode: Compilation (after elaboration, and WITHOUT committing files to disk)")
    cmd("commit")
      .foreach(_ => mode = "commit")
      .text("Mode: Committing to disk (after elaboration and compilation) [default]")
    cmd("lint")
      .foreach(_ => mode = "lint")
      .text("Mode: Linting (after elaboration, compilation, and committing to disk)")
      .children(optLinter)
  private def listDesignArgs: Unit =
    println("Design arguments:")
    val titles = f"${"Name"}%-20s${"Type"}%-20s${"Default"}%-20sDescription"
    println(titles)
    println("-" * titles.length)
    designArgs.values.foreach(a =>
      println(f"${a.name}%-20s${a.typeName}%-20s${a.valueStr}%-20s${a.desc}")
    )
  private def listBackends: Unit =
    println(
      s"""|Available languages (see their dialects below):
          |verilog - Verilog or SystemVerilog
          |vhdl    - VHDL
          |
          |Available Verilog/SystemVerilog dialects:
          |v2001   - Verilog 2001
          |sv2005  - SystemVerilog 2005 [default]
          |sv2012  - SystemVerilog 2012
          |sv2017  - SystemVerilog 2017
          |
          |Available VHDL dialects:
          |v93     - VHDL 1993
          |v2008   - VHDL 2008 [default]
          |v2019   - VHDL 2019""".stripMargin
    )
  end listBackends

  def main(args: Array[String]): Unit =
    if (parser.parse(args, ()).isDefined)
      mode match
        case "help" =>
          helpMode match
            case "backend"     => listBackends
            case "design-args" => listDesignArgs
            case _             => println(parser.usage)
        case "elaborate" => elaborate
        case "compile"   => elaborate.compile
        case "commit"    => elaborate.compile.commit
        case "lint"      => elaborate.compile.commit.lint
        case _           =>
  end main
end DFApp
