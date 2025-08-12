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
        new GowinDesignerProjectTimingConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile
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
val GowinDesignerProjectTimingConstraints =
  SourceType.Tool("GowinDesigner", "ProjectTimingConstraints")
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
    getSet.designDB.top.dclMeta.annotations.collectFirst {
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
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    s"""|create_project -name $topName -dir ../ -force -pn {$part} -device_version {$deviceVersion}
        |add_file $targetLanguage ${hdlFiles.mkString(" ")}
        |add_file $topName.cst
        |add_file $topName.sdc
        |set_option -top_module $topName
        |set_option -${targetLanguage}_std $std
        |set_option -use_cpu_as_gpio 1
        |run all
        |file copy -force ./impl/pnr/${topName}.fs ./${topName}.fs
        |""".stripMargin
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
  val topIOs = designDB.top.members(MemberView.Folded).collect {
    case dcl @ DclPort() => dcl
  }

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
    topIOs.view.flatMap(cstPortConstraints).toList

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

class GowinDesignerProjectTimingConstraintsPrinter(using getSet: MemberGetSet, co: CompilerOptions):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.sdc"
  val topIOs = designDB.top.members(MemberView.Folded).collect {
    case dcl @ DclPort() => dcl
  }

  def sdc_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    val portPattern =
      port.dfType match
        case DFBit | DFBool => portName
        case _              => constraint.bitIdx match
            case None   => s"$portName[*]"
            case bitIdx => s"$portName[$bitIdx]"
    s"[get_ports {$portPattern}]"

  // Vivado has a hard limit of ~200us for the clock period, even for virtual clocks
  val VivadoMaxClockPeriodNS = BigDecimal(200000)

  def sdcTimingIgnoreConstraint(
      port: DFVal.Dcl,
      constraint: constraints.Timing.Ignore
  ): String =
    def set_false_path(dir: String): String =
      s"set_false_path $dir ${sdc_get_ports(port, constraint)}"
    def set_io_delay(dir: String): String =
      constraint.maxFreqMinPeriod match
        case None                         => ""
        case maxFreqMinPeriod: RateNumber =>
          val maxFreqMinPeriodNS = maxFreqMinPeriod.to_ns.value.min(VivadoMaxClockPeriodNS)
          val virtualClockName = s"virtual_clock_${port.getName}"
          //format: off
          s"""|
              |create_clock -period $maxFreqMinPeriodNS -name $virtualClockName
              |set_${dir}_delay -clock [get_clocks $virtualClockName] -min 0.0 ${sdc_get_ports(port,constraint)}
              |set_${dir}_delay -clock [get_clocks $virtualClockName] -max 0.0 ${sdc_get_ports(port,constraint)}""".stripMargin
          //format: on
        case _ => ""
    (port.modifier.dir: @unchecked) match
      case DFVal.Modifier.IN =>
        set_false_path("-from") + set_io_delay("input")
      case DFVal.Modifier.OUT =>
        set_false_path("-to") + set_io_delay("output")
      // TODO: for INOUT, also check that its actually used in both directions by the design
      case DFVal.Modifier.INOUT => set_false_path("-from") + set_false_path("-to")
  end sdcTimingIgnoreConstraint

  def sdcTimingClockConstraint(
      port: DFVal.Dcl,
      constraint: constraints.Timing.Clock
  ): String =
    s"create_clock -add -name ${port.getName} -period ${constraint.rate.to_ns.value.bigDecimal.toPlainString} [get_ports {${port.getName}}]"
  end sdcTimingClockConstraint

  def sdcPortConstraints(
      port: DFVal.Dcl
  ): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.Timing.Ignore => sdcTimingIgnoreConstraint(port, constraint)
      case constraint: constraints.Timing.Clock  => sdcTimingClockConstraint(port, constraint)
    }
  end sdcPortConstraints

  def sdcPortConstraints: List[String] =
    topIOs.view.flatMap(sdcPortConstraints).toList

  def contents: String =
    s"""|${sdcPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      GowinDesignerProjectTimingConstraints,
      constraintsFileName,
      contents
    )
end GowinDesignerProjectTimingConstraintsPrinter
