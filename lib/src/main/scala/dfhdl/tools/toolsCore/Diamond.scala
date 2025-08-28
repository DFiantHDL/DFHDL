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

object Diamond extends Builder:
  val toolName: String = "Diamond"
  protected def binExec: String = "diamondc"
  override protected def windowsBinExec: String = "pnmainc.exe"
  protected def versionCmd: String = ???
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = (s""".*diamond\\${separatorChar}(\\d+\\.\\d+)""").r
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
        new DiamondProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile,
        new DiamondProjectPhysicalConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile,
        new DiamondProjectTimingConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile
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
    s"${topName}.ldf",
    s"${topName}.bit",
    s"${topName}1.sty"
    // s"impl${separatorChar}${topName}_process_config.json"
  )
end Diamond

val DiamondProjectTclConfig = SourceType.Tool("Diamond", "ProjectTclConfig")
val DiamondProjectTimingConstraints =
  SourceType.Tool("Diamond", "ProjectTimingConstraints")
val DiamondProjectPhysicalConstraints =
  SourceType.Tool("Diamond", "ProjectPhysicalConstraints")

class DiamondProjectTclConfigPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val (part, deviceVersion): (String, String) =
    designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.DeviceID => (annotation.partName, annotation.deviceVersion)
    }.getOrElse(throw new IllegalArgumentException("No device constraint found"))
  val verilogStandard: String = co.backend match
    case backend: backends.verilog =>
      val std = backend.dialect match
        case VerilogDialect.v95   => "Verilog 95"
        case VerilogDialect.v2001 => "Verilog 2001"
        case _                    => "System Verilog"
      s"""prj_impl option VerilogStandard {"$std"}"""
    case backend: backends.vhdl => ""
  val vhdl2008SynthStrategy: String = co.backend match
    case backend: backends.verilog => ""
    case backend: backends.vhdl    => backend.dialect match
        case VHDLDialect.v93                       => ""
        case VHDLDialect.v2008 | VHDLDialect.v2019 =>
          "prj_strgy set_value lse_vhdl2008=True"
  val hdlFiles: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.Design | SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) if !path.endsWith(".vh") && !path.endsWith(".svh") =>
      path.forceWindowsToLinuxPath
  }
  def activeDualPurposeGroups: List[String] =
    designDB.topIOs.view.flatMap(_.meta.annotations.collect {
      case constraint: constraints.IO =>
        constraint.dualPurposeGroups.toList.flatMap(_.split("/"))
    }).flatten.toList.distinct
  def gpioOptions: String =
    activeDualPurposeGroups.map(group => s"set_option -use_${group}_as_gpio 1")
      .mkString("\n").emptyOr("\n" + _)
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    //format: off
    //No need to add lpf file, because lattice does so automatically
    s"""|file delete -force ./impl1
        |prj_project new -name "$topName" -impl "impl1" -dev $part -synthesis "lse"${vhdl2008SynthStrategy.emptyOr("\n" + _)}${verilogStandard.emptyOr("\n" + _)}
        |prj_src add ${hdlFiles.map("\"" + _ + "\"").mkString(" ")}
        |prj_impl option top Demo
        |prj_project save
        |prj_src add "./$topName.ldc"
        |prj_run Export -impl impl1 -task Bitgen
        |prj_project save
        |prj_project close
        |file copy -force ./impl1/${topName}_impl1.bit ./${topName}.bit
        |""".stripMargin
    //format: on
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, DiamondProjectTclConfig, configFileName, contents)
end DiamondProjectTclConfigPrinter

class DiamondProjectPhysicalConstraintsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.lpf"

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
      addToDict("PULLMODE", pullModeStr)
    }

    val locate =
      s"""LOCATE COMP "${cst_get_ports(port, portConstraint)}" SITE "${portConstraint.loc}";"""
    val ioBuf =
      if (dict.nonEmpty) s"""IOBUF PORT "${cst_get_ports(port, portConstraint)}" $dict;"""
      else ""

    s"""${locate}${ioBuf.emptyOr("\n" + _)}"""
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
      DiamondProjectPhysicalConstraints,
      constraintsFileName,
      contents
    )
end DiamondProjectPhysicalConstraintsPrinter

class DiamondProjectTimingConstraintsPrinter(using getSet: MemberGetSet, co: CompilerOptions):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.ldc"

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
    designDB.topIOs.view.flatMap(sdcPortConstraints).toList

  def contents: String =
    s"""|${sdcPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      DiamondProjectTimingConstraints,
      constraintsFileName,
      contents
    )
end DiamondProjectTimingConstraintsPrinter
