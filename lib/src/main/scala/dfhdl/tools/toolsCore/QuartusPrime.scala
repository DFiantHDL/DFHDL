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

sealed abstract class QuartusPrime(pro: Boolean) extends Builder:
  val toolName: String = if (pro) "Quartus Prime Pro" else "QuartusPrime Lite/Standard"
  protected def binExec: String = "quartus_sh"
  protected def versionCmd: String = "-v"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val editionPattern = if (pro) ".*Pro Edition" else ".*(Lite|Standard) Edition"
    val versionPattern = s"""(?s)Quartus.*Version (\\d+\\.\\d+)$editionPattern""".r
    versionPattern.findFirstMatchIn(cmdRetStr).map(_.group(1))

  override protected[dfhdl] def buildPreprocess(cd: CompiledDesign)(using
      CompilerOptions,
      BuilderOptions
  ): CompiledDesign =
    addSourceFiles(
      cd,
      List(
        new QuartusPrimeProjectTclConfigPrinter(using cd.stagedDB.getSet).getSourceFile,
        new QuartusPrimeProjectPhysicalConstraintsPrinter(using cd.stagedDB.getSet).getSourceFile,
        new QuartusPrimeProjectWarningSuppressionsPrinter(using cd.stagedDB.getSet).getSourceFile,
        new BuilderProjectTimingConstraintsPrinter(
          ".sdc",
          enableDerivedClockUncertainty = true,
          enableToggleRateLimitIODelay = true
        )(using cd.stagedDB.getSet).getSourceFile
      ) ++ new QuartusPrimeIPPrinter(using cd.stagedDB.getSet).getSourceFiles
    )
  def build(
      cd: CompiledDesign
  )(using CompilerOptions, BuilderOptions): CompiledDesign =
    given MemberGetSet = cd.stagedDB.getSet
    exec(
      s"-t ${topName}.tcl"
    )
    cd
  override protected[dfhdl] def producedFiles(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      bo: TOptions
  ): List[String] = List(
    s"${topName}.qpf",
    s"${topName}.qsf",
    s"${topName}.sof"
  ) ++ (if (pro) Nil else List(s"${topName}.svf"))
end QuartusPrime

object QuartusPrime extends QuartusPrime(false)
object QuartusPrimePro extends QuartusPrime(true)

val QuartusPrimeProjectTclConfig = SourceType.Tool("QuartusPrime", "ProjectTclConfig")
val QuartusPrimeProjectPhysicalConstraints =
  SourceType.Tool("QuartusPrime", "ProjectPhysicalConstraints")
val QuartusPrimeProjectWarningSuppressions =
  SourceType.Tool("QuartusPrime", "ProjectWarningSuppressions")
val QuartusPrimeIP =
  SourceType.Tool("QuartusPrime", "IP")

class QuartusPrimeProjectTclConfigPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val pro: Boolean = designDB.top.dclMeta.annotations.collectFirst {
    case constraints.DeviceID(vendor = constraints.DeviceID.Vendor.AlteraIntel(pro)) => pro
  }.get
  val targetLanguage: String = co.backend match
    case _: backends.verilog => "verilog"
    case _: backends.vhdl    => "vhdl"
  val (part, deviceVersion): (String, String) =
    designDB.top.dclMeta.annotations.collectFirst {
      case annotation: constraints.DeviceID => (annotation.partName, annotation.deviceVersion)
    }.getOrElse(throw new IllegalArgumentException("No device constraint found"))
  val std: String = co.backend match
    case backend: backends.verilog => backend.dialect match
        case VerilogDialect.v95    => "Verilog_1995"
        case VerilogDialect.v2001  => "Verilog_2001"
        case VerilogDialect.sv2005 => "SystemVerilog_2005"
        case VerilogDialect.sv2009 => "SystemVerilog_2009"
        case _                     => "SystemVerilog_2012"
    case backend: backends.vhdl => backend.dialect match
        case VHDLDialect.v93   => "VHDL_1993"
        case VHDLDialect.v2008 => "VHDL_2008"
        case VHDLDialect.v2019 => "VHDL_2019"
  val hdlFiles: List[String] = designDB.srcFiles.collect {
    case SourceFile(
          SourceOrigin.Committed,
          SourceType.Design | SourceType.DFHDLDef | SourceType.GlobalDef,
          path,
          _
        ) =>
      path.forceWindowsToLinuxPath
  }
  def addHDLFilesCmd: String =
    val name = targetLanguage.toUpperCase()
    hdlFiles.map(file =>
      s"set_global_assignment -name ${name}_FILE $file -hdl_version $std"
    ).mkString("\n")
  def activeDualPurposeGroups: List[String] =
    designDB.topIOs.view.flatMap(_.meta.annotations.collect {
      case constraint: constraints.IO =>
        constraint.dualPurposeGroups.toList.flatMap(_.split("/"))
    }).flatten.toList.distinct
  def configFileName: String = s"$topName.tcl"
  def generateSVFFileCmd: String =
    if (pro) "" else "set_global_assignment -name GENERATE_SVF_FILE ON"
  def qsysIPs: String =
    designDB.uniqueDesignMemberList.collect {
      case (qsysIP: DFDesignBlock, _) if qsysIP.isQsysIPBlackbox =>
        s"catch {exec qsys-script --script=ips/${qsysIP.dclName}.tcl --quartus-project=${topName}}"
    }.mkString("\n")
  def contents: String =
    sn"""|load_package flow
         |project_new -overwrite -part $part $topName
         |$addHDLFilesCmd
         |set_global_assignment -name TOP_LEVEL_ENTITY $topName
         |source ${topName}_physical.tcl
         |set_global_assignment -name SDC_FILE $topName.sdc
         |$generateSVFFileCmd
         |project_close
         |$qsysIPs
         |project_open ${topName}
         |execute_flow -compile
         |project_close
         |"""
  def getSourceFile: SourceFile =
    SourceFile(SourceOrigin.Compiled, QuartusPrimeProjectTclConfig, configFileName, contents)
end QuartusPrimeProjectTclConfigPrinter

class QuartusPrimeProjectPhysicalConstraintsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"${topName}_physical.tcl"
  val pro: Boolean = designDB.top.dclMeta.annotations.collectFirst {
    case constraints.DeviceID(vendor = constraints.DeviceID.Vendor.AlteraIntel(pro)) => pro
  }.get
  val deviceInfo: constraints.DeviceInfo = designDB.top.dclMeta.annotations.collectFirst {
    case constraint: constraints.DeviceInfo => constraint
  }.getOrElse(throw new IllegalArgumentException("No device info found"))

  def qsf_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    port.dfType match
      case DFBit | DFBool => portName
      case _              => constraint.bitIdx match
          case None => throw new IllegalArgumentException(
              s"No bit index constraint found for port ${portName}"
            )
          case bitIdx => s"$portName[$bitIdx]"

  def qsfIOConstraint(
      port: DFVal.Dcl,
      portConstraint: constraints.IO
  ): String =
    var instanceAssignments = ""
    val portName = qsf_get_ports(port, portConstraint)
    def addInstanceAssignment(key: String, value: Any): Unit =
      if (instanceAssignments.nonEmpty)
        instanceAssignments += "\n"
      instanceAssignments += s"set_instance_assignment -name $key $value -to $portName"

    // IO standard constraint
    portConstraint.standard.foreach { standard =>
      val standardStr = (standard, portConstraint.levelVolt) match
        case (constraints.IO.Standard.LVCMOS, levelVolt: Double) =>
          if (pro)
            levelVolt match
              case 1.2 => "\"1.2-V\""
              case _   => s"\"$levelVolt-V LVCMOS\""
          else s"\"$levelVolt V\""
        case (constraints.IO.Standard.LVTTL, levelVolt: Double) =>
          if (pro) s"\"$levelVolt-V LVTTL\""
          else s"\"$levelVolt V LVTTL\""
        case (constraints.IO.Standard.SchmittTrigger, levelVolt: Double) =>
          s"\"$levelVolt V Schmitt Trigger\""
        case (constraints.IO.Standard.LVDS, _) => s"LVDS"
        case x                                 => throw new IllegalArgumentException(
            s"Invalid standard constraint for port ${port.getName}: $x"
          )
      addInstanceAssignment("IO_STANDARD", standardStr)
    }

    // Slew rate constraint
    portConstraint.slewRate.foreach { slewRate =>
      // TODO: what about other slew rate values?
      val slewRateStr = slewRate match
        case constraints.IO.SlewRate.SLOWEST => deviceInfo.slewRateSlowest.getOrElse(
            throw new IllegalArgumentException("Slowest slew rate not found in device info")
          ).toString
        case constraints.IO.SlewRate.FASTEST => deviceInfo.slewRateFastest.getOrElse(
            throw new IllegalArgumentException("Fastest slew rate not found in device info")
          ).toString
        case constraints.IO.SlewRate.CUSTOM(value) => value.toString
      addInstanceAssignment("SLEW_RATE", slewRateStr)
    }

    // Drive strength constraint
    portConstraint.driveStrength.foreach { driveStrength =>
      addInstanceAssignment("CURRENT_STRENGTH_NEW", s"${driveStrength}MA")
    }

    // Pull mode constraint
    portConstraint.pullMode.foreach { pullMode =>
      val pullModeStr = pullMode match
        case constraints.IO.PullMode.UP => "ON"
        case _                          => "OFF"
      addInstanceAssignment("WEAK_PULL_UP_RESISTOR", pullModeStr)
    }

    //format: off
    s"""|set_location_assignment PIN_${portConstraint.loc} -to $portName
        |$instanceAssignments""".stripMargin
    //format: on
  end qsfIOConstraint

  def qsfPortConstraints(
      port: DFVal.Dcl
  ): List[String] =
    port.meta.annotations.collect {
      case constraint: constraints.IO => qsfIOConstraint(port, constraint)
    }
  end qsfPortConstraints

  def qsfPortConstraints: List[String] =
    designDB.topIOs.view.flatMap(qsfPortConstraints).toList
  def qsfDeviceProperties: String =
    designDB.top.dclMeta.annotations.collect {
      case constraint: constraints.DeviceProperties =>
        constraint.properties.map {
          case (k, v) => s"set_global_assignment -name $k $v"
        }.mkString("\n")
    }.mkString("\n")
  end qsfDeviceProperties

  def contents: String =
    s"""|${qsfPortConstraints.mkString("\n")}
        |$qsfDeviceProperties
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      QuartusPrimeProjectPhysicalConstraints,
      constraintsFileName,
      contents
    )
end QuartusPrimeProjectPhysicalConstraintsPrinter

class QuartusPrimeProjectWarningSuppressionsPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  def configFileName: String = s"$topName.srf"
  val pro: Boolean = designDB.top.dclMeta.annotations.collectFirst {
    case constraints.DeviceID(vendor = constraints.DeviceID.Vendor.AlteraIntel(pro)) => pro
  }.get
  def warningSuppression(
      id: String,
      keyWord: String = "*",
      proExpected: ConfigN[Boolean] = None
  ): Option[String] =
    if (proExpected.nonEmpty && proExpected != pro) None
    else Some(s"""{ "" "" "" "$keyWord" {  } {  } 0 $id "" 0 0 "Design Software" 0 -1 0 ""}""")
  def warningSuppressions: Iterator[String] =
    Iterator(
      // Suppressing: "Number of processors has not been specified which may cause overloading on shared machines."
      // Reasoning: Not a valid reason for a warning. Oooh running a build takes a load.. You're kidding?!
      warningSuppression("18236", proExpected = false),
      // Suppressing: "Timing characteristics of device ... are preliminary"
      // Reasoning: The user can do nothing about this. It's like saying "the tool could have a bug we didn't find".
      warningSuppression("18291", proExpected = true),
      // Suppressing: "Feature LogicLock detected"
      // Reasoning: This is a default behavior for basic design. The user sees no relevant effect.
      warningSuppression("292013", "LogicLock"),
      // Suppressing: "Timing characteristics of device ... are preliminary"
      // Reasoning: The user can do nothing about this. It's like saying "the tool could have a bug we didn't find".
      warningSuppression("334000", proExpected = false),
      // Suppressing: "Clock uncertainty characteristics of the ... device family are preliminary"
      // Reasoning: The user can do nothing about this. It's like saying "the tool could have a bug we didn't find".
      warningSuppression("332158", proExpected = true)
    ).flatten
  def contents: String = warningSuppressions.mkString("\n")
  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      QuartusPrimeProjectWarningSuppressions,
      configFileName,
      contents
    )
end QuartusPrimeProjectWarningSuppressionsPrinter

class QuartusPrimeIPPrinter(using
    getSet: MemberGetSet,
    co: CompilerOptions,
    bo: BuilderOptions
):
  def contents(qsysIP: DFDesignBlock): String =
    val ipName = qsysIP.dclName
    val DFDesignBlock.InstMode.BlackBox(DFDesignBlock.InstMode.BlackBox.Source.Qsys(ipType)) =
      qsysIP.instMode: @unchecked
    val ipInstanceName = s"${ipType}_inst"
    val members = qsysIP.members(MemberView.Folded)
    val ipVersion = members.collectFirst {
      case param: DFVal.DesignParam if param.getName == "version" =>
        " " + param.dfValRef.get.getConstData.get.asInstanceOf[Option[String]].get
    }.getOrElse("")
    val ipParams = members.collect {
      case param: DFVal.DesignParam if param.getName != "version" =>
        s"set_instance_parameter_value $ipInstanceName {${param.getName}} {${param.dfValRef.get.getConstData.get.asInstanceOf[Option[Any]].get}}"
    }.mkString("\n")
    val ipExports = members.collect {
      case port @ DclPort() =>
        s"set_interface_property ${port.getName} EXPORT_OF $ipInstanceName.${port.getName}"
    }.mkString("\n")

    sn"""|package require qsys
         |
         |# create the system
         |create_system $ipName
         |# add HDL parameters
         |
         |# add the components
         |add_instance $ipInstanceName $ipType$ipVersion
         |$ipParams
         |set_instance_property $ipInstanceName AUTO_EXPORT true
         |
         |# add the exports
         |$ipExports
         |
         |set_module_property FILE {$ipName.ip}
         |set_module_property GENERATION_ID {0x00000000}
         |set_module_property NAME {$ipName}
         |
         |# save the system
         |sync_sysinfo_parameters
         |save_system $ipName
         |"""
  end contents
  def getSourceFiles: List[SourceFile] =
    getSet.designDB.uniqueDesignMemberList.collect {
      case (qsysIP: DFDesignBlock, _) if qsysIP.isQsysIPBlackbox =>
        SourceFile(
          SourceOrigin.Compiled,
          QuartusPrimeIP,
          Paths.get("ips").resolve(s"${qsysIP.dclName}.tcl").toString,
          contents(qsysIP)
        )
    }
  end getSourceFiles
end QuartusPrimeIPPrinter
