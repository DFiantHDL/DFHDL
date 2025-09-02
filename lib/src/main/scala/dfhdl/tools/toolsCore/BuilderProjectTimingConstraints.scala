package dfhdl.tools.toolsCore
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.printing.Printer
import java.io.File.separatorChar
import dfhdl.compiler.ir.PhysicalNumber.Ops.MHz
import dfhdl.compiler.ir.RateNumber
import dfhdl.compiler.ir.PhysicalNumber

val BuilderProjectTimingConstraints =
  SourceType.Tool("Builder", "ProjectTimingConstraints")

class BuilderProjectTimingConstraintsPrinter(
    fileSuffix: String,
    enableDerivedClockUncertainty: Boolean = false
)(using
    getSet: MemberGetSet,
    co: CompilerOptions
):
  val designDB: DB = getSet.designDB
  val topName: String = getSet.topName
  val constraintsFileName: String = s"$topName$fileSuffix"

  def sdc_get_ports(port: DFVal.Dcl, constraint: constraints.SigConstraint): String =
    val portName = port.getName
    val portPattern =
      port.dfType match
        case DFBit | DFBool => portName
        case _              => constraint.bitIdx match
            case None   => s"$portName[*]"
            case bitIdx => s"$portName[$bitIdx]"
    s"[get_ports {$portPattern}]"

  // TODO: Vivado has a hard limit of ~200us for the clock period, even for virtual clocks.
  // Could this be a limitation of all tools?
  val MaxClockPeriodNS = BigDecimal(200000)

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
          val maxFreqMinPeriodNS = maxFreqMinPeriod.to_ns.value.min(MaxClockPeriodNS)
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

  def deriveClockUncertainty: String =
    if (enableDerivedClockUncertainty) "\nderive_clock_uncertainty" else ""

  def contents: String =
    s"""|${sdcPortConstraints.mkString("\n")}$deriveClockUncertainty
        |""".stripMargin

  def getSourceFile: SourceFile =
    SourceFile(
      SourceOrigin.Compiled,
      BuilderProjectTimingConstraints,
      constraintsFileName,
      contents
    )
end BuilderProjectTimingConstraintsPrinter
