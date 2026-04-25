/** this package contains all the dfhdl hardware annotations
  */
package dfhdl.hw

import dfhdl.compiler.printing.{Printer, HasCodeString}
import scala.annotation.StaticAnnotation
import dfhdl.internals.*
import scala.annotation.Annotation
import dfhdl.compiler.ir
import dfhdl.core.*

object annotation:
  sealed abstract class HWAnnotation extends StaticAnnotation:
    val isActive: Boolean
    val asIR: ir.annotation.HWAnnotation

  extension (annotList: List[Annotation])
    def getActiveHWAnnotations: List[ir.annotation.HWAnnotation] = annotList.collect {
      case annot: HWAnnotation if annot.isActive => annot.asIR
    }

  object unused:
    /** `quiet` suppresses the unused warning for the tagged value.
      */
    final class quiet(val isActive: Boolean) extends HWAnnotation:
      def this() = this(true)
      val asIR: ir.annotation.Unused = ir.annotation.Unused.Quiet

    /** `keep` suppresses the unused warning, and also attempts to keep the tagged value.
      */
    final class keep(val isActive: Boolean) extends HWAnnotation:
      def this() = this(true)
      val asIR: ir.annotation.Unused = ir.annotation.Unused.Keep

    /** `prune` removes all the redundant paths until and including the tagged value.
      */
    final class prune(val isActive: Boolean) extends HWAnnotation:
      def this() = this(true)
      val asIR: ir.annotation.Unused = ir.annotation.Unused.Prune
  end unused

  final class pure(val isActive: Boolean) extends HWAnnotation:
    def this() = this(true)
    val asIR: ir.annotation.Pure.type = ir.annotation.Pure

  /** Flattening Mode:
    *   - transparent: $memberName
    *   - prefix: $ownerName$sep$memberName
    *   - suffix: $memberName$sep$ownerName
    */
  object flattenMode:
    final class transparent() extends HWAnnotation:
      val isActive: Boolean = true
      val asIR: ir.annotation.FlattenMode.Transparent.type = ir.annotation.FlattenMode.Transparent
    final class prefix(val sep: String) extends HWAnnotation:
      val isActive: Boolean = true
      val asIR: ir.annotation.FlattenMode.Prefix = ir.annotation.FlattenMode.Prefix(sep)
    final class suffix(val sep: String) extends HWAnnotation:
      val isActive: Boolean = true
      val asIR: ir.annotation.FlattenMode.Suffix = ir.annotation.FlattenMode.Suffix(sep)
end annotation

object constraints:
  sealed abstract class Constraint extends annotation.HWAnnotation:
    val isActive: Boolean = true
    val asIR: ir.constraints.Constraint
  sealed abstract class GlobalConstraint extends Constraint:
    val asIR: ir.constraints.GlobalConstraint
  sealed abstract class SigConstraint extends Constraint:
    val bitIdx: ir.ConfigN[Int]
  final case class deviceID(
      vendor: deviceID.Vendor,
      deviceName: String,
      partName: String,
      deviceVersion: String
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceID =
      ir.constraints.DeviceID(vendor, deviceName, partName, deviceVersion)
  object deviceID:
    export ir.constraints.DeviceID.Vendor
  final case class deviceInfo(
      slewRateSlowest: ir.ConfigN[Int] = None,
      slewRateFastest: ir.ConfigN[Int] = None
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceInfo =
      ir.constraints.DeviceInfo(slewRateSlowest, slewRateFastest)
  final case class deviceProperties(properties: (String, String)*) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceProperties = ir.constraints.DeviceProperties(properties.toMap)
  final case class toolOptions(options: (String, String)*) extends GlobalConstraint:
    val asIR: ir.constraints.ToolOptions = ir.constraints.ToolOptions(options.toMap)
  final case class deviceConfig(
      flashPartName: String,
      interface: deviceConfig.Interface,
      sizeLimitMb: Int,
      masterRate: ir.ConfigN[ir.RateNumber] = None
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceConfig =
      ir.constraints.DeviceConfig(flashPartName, interface, sizeLimitMb, masterRate)
  object deviceConfig:
    export ir.constraints.DeviceConfig.Interface

  final case class io(
      bitIdx: ir.ConfigN[Int] = None,
      loc: ir.ConfigN[String] = None,
      dir: ir.ConfigN[io.Dir] = None,
      levelVolt: ir.ConfigN[io.LevelVolt] = None,
      standard: ir.ConfigN[io.Standard] = None,
      slewRate: ir.ConfigN[io.SlewRate] = None,
      driveStrength: ir.ConfigN[Int] = None,
      pullMode: ir.ConfigN[io.PullMode] = None,
      dualPurposeGroups: ir.ConfigN[String] = None,
      unusedPullMode: ir.ConfigN[io.PullMode] = None,
      invertActiveState: ir.ConfigN[Boolean] = None,
      missingPullDownSupport: ir.ConfigN[Boolean] = None
  ) extends SigConstraint:
    val asIR: ir.constraints.IO =
      ir.constraints.IO(
        bitIdx, loc, dir, levelVolt, standard, slewRate, driveStrength, pullMode, dualPurposeGroups,
        unusedPullMode, invertActiveState, missingPullDownSupport
      )
  end io
  object io:
    export ir.constraints.IO.{LevelVolt, Standard, SlewRate, PullMode, Dir}

  object timing:
    type InclusionPolicy = ir.ClkRstInclusionPolicy
    final val InclusionPolicy = ir.ClkRstInclusionPolicy

    final case class ignore(
        bitIdx: ir.ConfigN[Int] = None,
        maxFreqMinPeriod: ir.ConfigN[ir.RateNumber] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Ignore =
        ir.constraints.Timing.Ignore(bitIdx, maxFreqMinPeriod)

    final case class clock(
        rate: ir.ConfigN[ir.RateNumber] = None,
        edge: ir.ConfigN[ir.ClkCfg.Edge] = None,
        portName: ir.ConfigN[String] = None,
        inclusionPolicy: ir.ConfigN[ir.ClkRstInclusionPolicy] = None,
        grpName: ir.ConfigN[String] = None,
        bitIdx: ir.ConfigN[Int] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Clock =
        ir.constraints.Timing.Clock(rate, edge, portName, inclusionPolicy, grpName, bitIdx)
    object clock:
      export ir.ClkCfg.Edge
      type Rate = DFConstOf[DFTime | DFFreq]

    final case class reset(
        mode: ir.ConfigN[ir.RstCfg.Mode] = None,
        active: ir.ConfigN[ir.RstCfg.Active] = None,
        portName: ir.ConfigN[String] = None,
        inclusionPolicy: ir.ConfigN[ir.ClkRstInclusionPolicy] = None,
        bitIdx: ir.ConfigN[Int] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Reset =
        ir.constraints.Timing.Reset(mode, active, portName, inclusionPolicy, bitIdx)
    object reset:
      export ir.RstCfg.{Mode, Active}

    final case class related(domainContainer: RTDomainContainer)(using DFC) extends Constraint:
      val asIR: ir.constraints.Timing.Related =
        ir.constraints.Timing.Related(
          domainContainer.containedOwner.asIR
            .asInstanceOf[ir.DomainBlock | ir.DFDesignBlock]
            .refTW[ir.DomainBlock]
        )
  end timing
end constraints
