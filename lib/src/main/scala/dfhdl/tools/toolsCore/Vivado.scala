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
import dfhdl.compiler.ir.PhysicalNumber.Ops.MHz

object Vivado extends Builder, Programmer:
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
        new VivadoProjectConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile,
        new BuilderProjectTimingConstraintsPrinter("_timing.xdc")(using
          cd.stagedDB.getSet
        ).getSourceFile
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
  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      bo: TOptions
  ): List[String] = List(
    s"${topName}.bit",
    s"${topName}.xpr"
  ) ++ (if (bo.flash) List(s"${topName}.mcs") else Nil)
  override protected[dfhdl] def programPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      ProgrammerOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(
        new VivadoProgramScriptPrinter(using cd.stagedDB.getSet).getSourceFile
      )
    )
  def program(
      cd: CompiledDesign
  )(using CompilerOptions, ProgrammerOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(
      s"-mode batch -script ${topName}_prog.tcl"
    )
    cd
end Vivado

val VivadoProjectTclConfig = SourceType.Tool("Vivado", "ProjectTclConfig")
val VivadoProjectConstraints = SourceType.Tool("Vivado", "ProjectConstraints")
val VivadoProgramScript = SourceType.Tool("Vivado", "ProgramScript")

class VivadoProjectTclConfigPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val targetLanguage: String = co.backend match
    case _: backends.verilog => "Verilog"
    case _: backends.vhdl    => "VHDL"
  val part: String = getSet.designDB.top.dclMeta.annotations.collectFirst {
    case annotation: constraints.DeviceID => annotation.partName
  }.getOrElse(throw new IllegalArgumentException("No device constraint found"))
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
  def flashCmd: String =
    val config = designDB.top.dclMeta.annotations.collectFirst {
      case configConstraint: constraints.DeviceConfig => configConstraint
    }.getOrElse(throw new IllegalArgumentException("No `@deviceConfig` constraint found"))
    if (bo.flash)
      s"""\nwrite_cfgmem -format mcs -interface ${config.interface} -size ${config.sizeLimitMB} -loadbit "up 0x0 ./${topName}.bit" -file ./${topName}.mcs"""
    else ""
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    s"""|create_project $topName . -part $part -force
        |set_property target_language $targetLanguage [current_project]
        |add_files -norecurse ${hdlFiles.mkString("{\n  ", "\n  ", "\n}")}
        |set_property file_type {${fileType}} [get_files  *]
        |set_property top $topName [current_fileset]
        |add_files -fileset constrs_1 -norecurse {./${topName}.xdc  ./${topName}_timing.xdc}
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
        |write_bitstream -file ./$topName.bit -force$flashCmd
        |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectTclConfig, configFileName, contents)
end VivadoProjectTclConfigPrinter

class VivadoProjectConstraintsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName.xdc"

  def xdcDesignConstraints: List[String] =
    designDB.top.dclMeta.annotations.flatMap {
      case constraint: constraints.DeviceProperties =>
        constraint.properties.map {
          case (k, v) => s"set_property $k $v [current_design]"
        }
      case constraint: constraints.DeviceConfig =>
        import constraints.DeviceConfig.Interface.*
        val spiBusWidth = constraint.interface match
          case MasterSPI(busWidth) => Some(busWidth)
          case _                   => None
        val configMode = constraint.interface match
          case MasterSPI(busWidth)  => s"SPIx$busWidth"
          case MasterBPI(busWidth)  => s"BPI$busWidth"
          case SlaveSerial          => "S_SERIAL"
          case MasterSerial         => "M_SERIAL"
          case SlaveSMAP(8)         => "S_SELECTMAP"
          case MasterSMAP(8)        => "M_SELECTMAP"
          case SlaveSMAP(busWidth)  => s"S_SELECTMAP$busWidth"
          case MasterSMAP(busWidth) => s"M_SELECTMAP$busWidth"
        val configRate = constraint.masterRate match
          case None             => None
          case rate: RateNumber => Some((rate.to_freq / 1.MHz).value.toInt)
        val compress = if (bo.compress) "TRUE" else "FALSE"
        List(
          s"set_property CONFIG_MODE $configMode [current_design]",
          s"set_property BITSTREAM.GENERAL.COMPRESS $compress [current_design]"
        ) ++ spiBusWidth.map(w =>
          s"set_property BITSTREAM.CONFIG.SPI_BUSWIDTH $w [current_design]"
        ) ++ configRate.map(r => s"set_property BITSTREAM.CONFIG.CONFIGRATE $r [current_design]")
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
      val standardStr = standard.withLevelVolt(portConstraint.levelVolt.getOrElse(
        throw new IllegalArgumentException(
          s"No level constraint found for port ${port.getName}"
        )
      ))
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
      addToDict("PULLTYPE", pullModeStr)
    }

    s"set_property -dict {$dict} ${xdc_get_ports(port, portConstraint)}"
  end xdcIOConstraint

  def xdcPortConstraints(
      port: DFVal.Dcl
  ): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.IO => xdcIOConstraint(port, constraint)
    }
  end xdcPortConstraints

  def xdcPortConstraints: List[String] =
    designDB.topIOs.view.flatMap(xdcPortConstraints).toList

  def contents: String =
    s"""|${xdcDesignConstraints.mkString("\n")}
        |${xdcPortConstraints.mkString("\n")}
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProjectConstraints, constraintsFileName, contents)
end VivadoProjectConstraintsPrinter

class VivadoProgramScriptPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    po: ProgrammerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val config = designDB.top.dclMeta.annotations.collectFirst {
    case configConstraint: constraints.DeviceConfig => configConstraint
  }.getOrElse(throw new IllegalArgumentException("No `@deviceConfig` constraint found"))
  def configFileName: String = s"${topName}_prog.tcl"
  val progOrFlash: String =
    if (po.flash)
      s"""|create_hw_cfgmem -hw_device [current_hw_device] [lindex [get_cfgmem_parts {${config.flashPartName}}] 0]
          |set_property PROGRAM.BLANK_CHECK   0 [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.ERASE         1 [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.CFG_PROGRAM   1 [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.VERIFY        1 [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.CHECKSUM      0 [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.ADDRESS_RANGE {use_file} [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.FILES         [list "./$topName.mcs"] [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |set_property PROGRAM.UNUSED_PIN_TERMINATION {pull-none} [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |startgroup
          |create_hw_bitstream -hw_device [current_hw_device] [get_property PROGRAM.HW_CFGMEM_BITFILE [current_hw_device]]
          |program_hw_devices [current_hw_device]
          |refresh_hw_device [current_hw_device]
          |program_hw_cfgmem -hw_cfgmem [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
          |endgroup""".stripMargin
    else
      s"""|set_property PROGRAM.FILE ./$topName.bit [current_hw_device]
          |program_hw_devices [current_hw_device]""".stripMargin
  def contents: String =
    s"""|open_hw_manager
        |connect_hw_server
        |current_hw_target
        |open_hw_target
        |$progOrFlash
        |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, VivadoProgramScript, configFileName, contents)
end VivadoProgramScriptPrinter
