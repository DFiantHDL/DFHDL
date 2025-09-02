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

object QuartusPrime extends Builder:
  val toolName: String = "QuartusPrime"
  protected def binExec: String = "quartus_sh"
  protected def versionCmd: String = "-v"
  protected def extractVersion(cmdRetStr: String): Option[String] =
    val versionPattern = """(?s)Quartus.*Version (\d+\.\d+)""".r
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
          enableDerivedClockUncertainty = true
        )(using cd.stagedDB.getSet).getSourceFile
      )
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
    s"${topName}.sof",
    s"${topName}.svf"
  )
end QuartusPrime

val QuartusPrimeProjectTclConfig = SourceType.Tool("QuartusPrime", "ProjectTclConfig")
val QuartusPrimeProjectPhysicalConstraints =
  SourceType.Tool("QuartusPrime", "ProjectPhysicalConstraints")
val QuartusPrimeProjectWarningSuppressions =
  SourceType.Tool("QuartusPrime", "ProjectWarningSuppressions")

class QuartusPrimeProjectTclConfigPrinter(using
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
  def addHDLFilesCmd: String =
    val name = targetLanguage.toUpperCase()
    hdlFiles.map(file => s"set_global_assignment -name ${name}_FILE $file").mkString("\n")
  def activeDualPurposeGroups: List[String] =
    designDB.topIOs.view.flatMap(_.meta.annotations.collect {
      case constraint: constraints.IO =>
        constraint.dualPurposeGroups.toList.flatMap(_.split("/"))
    }).flatten.toList.distinct
  def configFileName: String = s"$topName.tcl"
  def contents: String =
    s"""|load_package flow
        |project_new -overwrite -part $part $topName
        |$addHDLFilesCmd
        |set_global_assignment -name TOP_LEVEL_ENTITY $topName
        |source ${topName}_physical.tcl
        |set_global_assignment -name SDC_FILE $topName.sdc
        |set_global_assignment -name GENERATE_SVF_FILE ON
        |execute_flow -compile
        |project_close
        |""".stripMargin
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
        case (constraints.IO.Standard.LVCMOS, levelVolt: Double) => s"\"$levelVolt V\""
        case (constraints.IO.Standard.LVTTL, levelVolt: Double)  => s"\"$levelVolt V LVTTL\""
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
        case constraints.IO.SlewRate.SLOW => "0"
        case constraints.IO.SlewRate.FAST => "2"
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
  def warningSuppression(id: String, keyWord: String = "*"): String =
    s"""{ "" "" "" "$keyWord" {  } {  } 0 $id "" 0 0 "Design Software" 0 -1 0 ""}"""
  def contents: String =
    s"""|${warningSuppression("18236")}
        |${warningSuppression("292013", "LogicLock")}
        |${warningSuppression("334000")}
        |""".stripMargin
  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      QuartusPrimeProjectWarningSuppressions,
      configFileName,
      contents
    )
end QuartusPrimeProjectWarningSuppressionsPrinter
