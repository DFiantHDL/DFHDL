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
      vendor: ir.constraints.DeviceID.Vendor,
      deviceName: String,
      partName: String,
      deviceVersion: String
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceID =
      ir.constraints.DeviceID(vendor, deviceName, partName, deviceVersion)
  object deviceID:
    export ir.constraints.DeviceID.Vendor
  final case class deviceProperties(properties: (String, String)*) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceProperties = ir.constraints.DeviceProperties(properties.toMap)
  final case class toolOptions(options: (String, String)*) extends GlobalConstraint:
    val asIR: ir.constraints.ToolOptions = ir.constraints.ToolOptions(options.toMap)
  final case class deviceConfig(
      flashPartName: String,
      interface: ir.constraints.DeviceConfig.Interface,
      sizeLimitMB: Int
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceConfig =
      ir.constraints.DeviceConfig(flashPartName, interface, sizeLimitMB)
  object deviceConfig:
    export ir.constraints.DeviceConfig.Interface

  final case class io(
      bitIdx: ir.ConfigN[Int] = None,
      loc: ir.ConfigN[String] = None,
      levelVolt: ir.ConfigN[io.LevelVolt] = None,
      standard: ir.ConfigN[io.Standard] = None,
      slewRate: ir.ConfigN[io.SlewRate] = None,
      driveStrength: ir.ConfigN[Int] = None,
      pullMode: ir.ConfigN[io.PullMode] = None,
      dualPurposeGroups: ir.ConfigN[String] = None
  ) extends SigConstraint:
    val asIR: ir.constraints.IO =
      ir.constraints.IO(
        bitIdx, loc, levelVolt, standard, slewRate, driveStrength, pullMode, dualPurposeGroups
      )
  end io
  object io:
    export ir.constraints.IO.{LevelVolt, Standard, SlewRate, PullMode}

  object timing:
    final case class ignore(
        bitIdx: ir.ConfigN[Int] = None,
        maxFreqMinPeriod: ir.ConfigN[ir.RateNumber] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Ignore =
        ir.constraints.Timing.Ignore(bitIdx, maxFreqMinPeriod)

    final case class clock(
        rate: ir.RateNumber
    ) extends Constraint:
      val asIR: ir.constraints.Timing.Clock = ir.constraints.Timing.Clock(rate)
  end timing
end constraints
