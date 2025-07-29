package dfhdl.tools.toolsCore

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.{PrinterOptions, CompilerOptions, BuilderOptions, ToolOptions}
import dfhdl.compiler.printing.Printer
import dfhdl.compiler.analysis.*
import java.nio.file.Paths
import dfhdl.backends
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.constraints
import dfhdl.compiler.ir.RateNumber

object Vivado extends Builder:
  val toolName: String = "Vivado"
  protected def binExec: String = "vivado"
  override protected def windowsBinExec: String = "vivado.bat"
  protected def versionCmd: String = s"-version"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """vivado\s+v(\d+\.\d+)""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(
        new VivadoProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile,
        new VivadoProjectConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile
      )
    )
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(
      s"-mode batch -script ${topName}.tcl"
    )
    cd
end Vivado

val VivadoProjectTclConfig = SourceType.Tool("Vivado", "ProjectTclConfig")
val VivadoProjectConstraints = SourceType.Tool("Vivado", "ProjectConstraints")

class VivadoProjectTclConfigPrinter(using getSet: MemberGetSet, co: CompilerOptions):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val targetLanguage: String = co.backend match
    case _: backends.verilog => "Verilog"
    case _: backends.vhdl    => "VHDL"
  val part: String = getSet.designDB.top.dclMeta.annotations.collectFirst {
    case annotation: constraints.Device => annotation.name
  }.getOrElse(throw new IllegalArgumentException("No device annotation found"))
  val fileType: String = co.backend match
    case backend: backends.verilog => backend.dialect match
        case VerilogDialect.v95 | VerilogDialect.v2001 => "Verilog"
        case _                                         => "SystemVerilog"
    case backend: backends.vhdl => backend.dialect match
        case VHDLDialect.v93   => "VHDL"
        case VHDLDialect.v2008 => "VHDL 2008"
        case VHDLDialect.v2019 => "VHDL 2019"
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
    s"""|create_project $topName . -part $part -force
        |set_property target_language $targetLanguage [current_project]
        |add_files -norecurse ${hdlFiles.mkString("{\n  ", "\n  ", "\n}")}
        |set_property file_type {${fileType}} [get_files  *]
        |add_files -fileset constrs_1 -norecurse ./${topName}.xdc
        |######################################################################
        |# Suppress warnings
        |######################################################################
        |# the warning "Parallel synthesis criteria is not met" should not be a warning
        |set_msg_config -id {Synth 8-7080} -new_severity {INFO}
        |# redundant warning that default value for parameter is not defined
        |set_msg_config -suppress -id {Synth 8-9661} 
        |# bug in 2024.1
        |if {[version -short] == "2024.1"} {
        |    set_msg_config -suppress -id {Device 21-9320} -string {{WARNING: [Device 21-9320] Failed to find the Oracle tile group with name 'HSR_BOUNDARY_TOP'. This is required for Clock regions and Virtual grid.}} 
        |    set_msg_config -suppress -id {Device 21-2174} -string {{WARNING: [Device 21-2174] Failed to initialize Virtual grid.}} 
        |}
        |######################################################################
        |launch_runs impl_1
        |wait_on_run impl_1
        |open_run impl_1
        |write_bitstream -file ./$topName.bit -force
        |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectTclConfig, configFileName, contents)
end VivadoProjectTclConfigPrinter

class VivadoProjectConstraintsPrinter(using getSet: MemberGetSet, co: CompilerOptions):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.xdc"
  val topIOs = designDB.top.members(MemberView.Folded).collect {
    case dcl @ DclPort() => dcl
  }

  def xdcDesignConstraints: List[String] =
    designDB.top.dclMeta.annotations.flatMap {
      case deviceConstraint: constraints.Device =>
        deviceConstraint.properties.map {
          case (k, v) => s"set_property $k $v [current_design]"
        }
      case _ => Nil
    }.toList

  def xdc_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    val portPattern =
      port.dfType match
        case DFBit | DFBool => portName
        case _              => constraint.bitIdx match
            case None   => s"$portName[*]"
            case bitIdx => s"$portName[$bitIdx]"
    s"[get_ports {$portPattern}]"

  def xdcIOConstraint(
      port: DFVal.Dcl,
      portConstraint: constraints.IO
  ): String =
    var dict = ""
    def addToDict(key: String, value: Any): Unit =
      if (dict.nonEmpty)
        dict += "  "
      dict += s"$key $value"

    // Location constraint
    portConstraint.loc.foreach { loc =>
      addToDict("PACKAGE_PIN", loc)
    }

    // IO standard constraint
    portConstraint.standard.foreach { standard =>
      val standardStr = standard match
        case constraints.IO.Standard.LVCMOS33 => "LVCMOS33"
        case constraints.IO.Standard.LVCMOS25 => "LVCMOS25"
        case constraints.IO.Standard.LVCMOS18 => "LVCMOS18"
      addToDict("IOSTANDARD", standardStr)
    }

    // Slew rate constraint
    portConstraint.slewRate.foreach { slewRate =>
      val slewRateStr = slewRate match
        case constraints.IO.SlewRate.SLOW => "SLOW"
        case constraints.IO.SlewRate.FAST => "FAST"
      addToDict("SLEW", slewRateStr)
    }

    // Drive strength constraint
    portConstraint.driveStrength.foreach { driveStrength =>
      addToDict("DRIVE", driveStrength)
    }

    // Pull mode constraint
    portConstraint.pullMode.foreach { pullMode =>
      val pullModeStr = pullMode match
        case constraints.IO.PullMode.UP   => "PULLUP"
        case constraints.IO.PullMode.DOWN => "PULLDOWN"
      addToDict("PULLMODE", pullModeStr)
    }

    s"set_property -dict {$dict} ${xdc_get_ports(port, portConstraint)}"
  end xdcIOConstraint

  // Vivado has a hard limit of ~200us for the clock period, even for virtual clocks
  val VivadoMaxClockPeriodNS = BigDecimal(200000)

  def xdcTimingIgnoreConstraint(
      port: DFVal.Dcl,
      constraint: constraints.Timing.Ignore
  ): String =
    def set_false_path(dir: String): String =
      s"set_false_path $dir ${xdc_get_ports(port, constraint)}"
    def set_io_delay(dir: String): String =
      constraint.maxFreqMinPeriod match
        case None                         => ""
        case maxFreqMinPeriod: RateNumber =>
          val maxFreqMinPeriodNS = maxFreqMinPeriod.to_ns.value.min(VivadoMaxClockPeriodNS)
          val virtualClockName = s"virtual_clock_${port.getName}"
          //format: off
          s"""|
              |create_clock -period $maxFreqMinPeriodNS -name $virtualClockName
              |set_${dir}_delay -clock [get_clocks $virtualClockName] -min 0.0 ${xdc_get_ports(port,constraint)}
              |set_${dir}_delay -clock [get_clocks $virtualClockName] -max 0.0 ${xdc_get_ports(port,constraint)}""".stripMargin
          //format: on
        case _ => ""
    (port.modifier.dir: @unchecked) match
      case DFVal.Modifier.IN =>
        set_false_path("-from") + set_io_delay("input")
      case DFVal.Modifier.OUT =>
        set_false_path("-to") + set_io_delay("output")
      // TODO: for INOUT, also check that its actually used in both directions by the design
      case DFVal.Modifier.INOUT => set_false_path("-from") + set_false_path("-to")
  end xdcTimingIgnoreConstraint

  def xdcTimingClockConstraint(
      port: DFVal.Dcl,
      constraint: constraints.Timing.Clock
  ): String =
    s"create_clock -add -name ${port.getName} -period ${constraint.rate.to_ns.value.bigDecimal.toPlainString} [get_ports {${port.getName}}]"
  end xdcTimingClockConstraint

  def xdcPortConstraints(
      port: DFVal.Dcl
  ): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.IO            => xdcIOConstraint(port, constraint)
      case constraint: constraints.Timing.Ignore => xdcTimingIgnoreConstraint(port, constraint)
      case constraint: constraints.Timing.Clock  => xdcTimingClockConstraint(port, constraint)
    }
  end xdcPortConstraints

  def xdcPortConstraints: List[String] =
    topIOs.view.flatMap(xdcPortConstraints).toList

  def contents: String =
    s"""|${xdcDesignConstraints.mkString("\n")}
        |${xdcPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectConstraints, constraintsFileName, contents)
end VivadoProjectConstraintsPrinter
