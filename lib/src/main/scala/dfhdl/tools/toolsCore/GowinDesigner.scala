package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.*
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import dfhdl.backends
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.constraints
import dfhdl.compiler.ir.RateNumber
import java.io.File.separatorChar

object GowinDesigner extends Builder:
  val toolName: String = "GowinDesigner"
  protected def binExec: String = "gw_sh"
  override protected def windowsBinExec: String = "gw_sh"
  protected def versionCmd: String = ???
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """.*Gowin_V(\d+\.\d+\.\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  // gowin designer does not have a version command, so we need to extract the version from installed path
  override private[dfhdl] lazy val installedVersion: Option[String] =
    if (runExecFullPath.isEmpty) None
    else extractVersion(runExecFullPath)

  override protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(
        new GowinDesignerProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile,
        new GowinDesignerProjectPhysicalConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile,
        new BuilderProjectTimingConstraintsPrinter(".sdc")(using cd.stagedDB.getSet).getSourceFile
      )
    )
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(
      s"${topName}.tcl"
    )
    cd
  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      bo: TOptions
  ): List[String] = List(
    s"${topName}.gprj",
    s"${topName}.fs",
    s"impl${separatorChar}${topName}_process_config.json"
  )
end GowinDesigner

val GowinDesignerProjectTclConfig = SourceType.Tool("GowinDesigner", "ProjectTclConfig")
val GowinDesignerProjectPhysicalConstraints =
  SourceType.Tool("GowinDesigner", "ProjectPhysicalConstraints")

class GowinDesignerProjectTclConfigPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val targetLanguage: String = co.backend match
    case _: backends.verilog => "verilog"
    case _: backends.vhdl    => "vhdl"
  val (part, deviceVersion): (String, String) =
    designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.DeviceID => (annotation.partName, annotation.deviceVersion)
    }.getOrElse(throw new IllegalArgumentException("No device constraint found"))
  val std: String = co.backend match
    case backend: backends.verilog => backend.dialect match
        case VerilogDialect.v95   => "v1995"
        case VerilogDialect.v2001 => "v2001"
        case _                    => "sysv2017"
    case backend: backends.vhdl => backend.dialect match
        case VHDLDialect.v93   => "vhd1993"
        case VHDLDialect.v2008 => "vhd2008"
        case VHDLDialect.v2019 => "vhd2019"
  val hdlFiles: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.Design | SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) =>
      path.forceWindowsToLinuxPath
  }
  def activeDualPurposeGroups: List[String] =
    designDB.topIOs.view.flatMap(_.meta.annotations.collect {
      case constraint: constraints.IO =>
        constraint.dualPurposeGroups.toList.flatMap(_.split("/"))
    }).flatten.toList.distinct
  def gpioOptions: String =
    activeDualPurposeGroups.map(group => s"set_option -use_${group}_as_gpio 1").mkString("\n")
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    sn"""|create_project -name $topName -dir ../ -force -pn {$part} -device_version {$deviceVersion}
         |add_file $targetLanguage ${hdlFiles.mkString(" ")}
         |add_file $topName.cst
         |add_file $topName.sdc
         |set_option -top_module $topName
         |set_option -${targetLanguage}_std $std
         |$gpioOptions
         |run all
         |file copy -force ./impl/pnr/${topName}.fs ./${topName}.fs
         |"""
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, GowinDesignerProjectTclConfig, configFileName, contents)
end GowinDesignerProjectTclConfigPrinter

class GowinDesignerProjectPhysicalConstraintsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.cst"

  def cst_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    port.dfType match
      case DFBit | DFBool => portName
      case _              => constraint.bitIdx match
          case None => throw new IllegalArgumentException(
              s"No bit index constraint found for port ${portName}"
            )
          case bitIdx => s"$portName[$bitIdx]"

  def cstIOConstraint(
      port: DFVal.Dcl,
      portConstraint: constraints.IO
  ): String =
    var dict = ""
    def addToDict(key: String, value: Any): Unit =
      if (dict.nonEmpty)
        dict += "  "
      dict += s"$key=$value"

    // IO standard constraint
    portConstraint.standard.foreach { standard =>
      val standardStr = standard.withLevelVolt(portConstraint.levelVolt.getOrElse(
        throw new IllegalArgumentException(
          s"No level constraint found for port ${port.getName}"
        )
      ))
      addToDict("IO_TYPE", standardStr)
    }

    // Drive strength constraint
    portConstraint.driveStrength.foreach { driveStrength =>
      addToDict("DRIVE", driveStrength)
    }

    // Pull mode constraint
    portConstraint.pullMode.foreach { pullMode =>
      val pullModeStr = pullMode match
        case constraints.IO.PullMode.UP   => "UP"
        case constraints.IO.PullMode.DOWN => "DOWN"
      addToDict("PULL_MODE", pullModeStr)
    }

    s"""|IO_LOC "${cst_get_ports(port, portConstraint)}" ${portConstraint.loc};
        |IO_PORT "${cst_get_ports(port, portConstraint)}" $dict;""".stripMargin
  end cstIOConstraint

  def cstPortConstraints(
      port: DFVal.Dcl
  ): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.IO => cstIOConstraint(port, constraint)
    }
  end cstPortConstraints

  def cstPortConstraints: List[String] =
    designDB.topIOs.view.flatMap(cstPortConstraints).toList

  def contents: String =
    s"""|${cstPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      GowinDesignerProjectPhysicalConstraints,
      constraintsFileName,
      contents
    )
end GowinDesignerProjectPhysicalConstraintsPrinter
