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
      vendor: deviceID.vendor,
      deviceName: String,
      partName: String,
      deviceVersion: String
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceID =
      ir.constraints.DeviceID(vendor(deviceID.vendor), deviceName, partName, deviceVersion)
  object deviceID:
    import ir.constraints.DeviceID.Vendor
    type vendor = vendor.type => Vendor
    object vendor:
      export Vendor.XilinxAMD as xilinxamd
      export Vendor.Lattice as lattice
      export Vendor.Gowin as gowin
      export Vendor.AlteraIntel as alteraintel
      export Vendor.TinyTapeout as tinytapeout
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
      interface: deviceConfig.interface,
      sizeLimitMb: Int,
      masterRate: ir.ConfigN[ir.RateNumber] = None
  ) extends GlobalConstraint:
    val asIR: ir.constraints.DeviceConfig =
      ir.constraints.DeviceConfig(
        flashPartName, interface(deviceConfig.interface), sizeLimitMb, masterRate
      )
  object deviceConfig:
    import ir.constraints.DeviceConfig.Interface
    type interface = interface.type => Interface
    object interface:
      export Interface.MasterSMAP as mastersmap
      export Interface.SlaveSMAP as slavesmap
      export Interface.MasterSPI as masterspi
      export Interface.MasterBPI as masterbpi
      export Interface.SlaveSerial as slaveserial
      export Interface.MasterSerial as masterserial

  final case class io(
      bitIdx: ir.ConfigN[Int] = None,
      loc: ir.ConfigN[String] = None,
      dir: io.dir = None,
      levelVolt: ir.ConfigN[io.LevelVolt] = None,
      standard: io.standard = None,
      slewRate: io.slewRate = None,
      driveStrength: ir.ConfigN[Int] = None,
      pullMode: io.pullMode = None,
      dualPurposeGroups: ir.ConfigN[String] = None,
      unusedPullMode: io.pullMode = None,
      invertActiveState: ir.ConfigN[Boolean] = None,
      missingPullDownSupport: ir.ConfigN[Boolean] = None
  ) extends SigConstraint:
    val asIR: ir.constraints.IO =
      ir.constraints.IO(
        bitIdx, loc, dir(io.dir), levelVolt, standard(io.standard), slewRate(io.slewRate),
        driveStrength, pullMode(io.pullMode), dualPurposeGroups, unusedPullMode(io.pullMode),
        invertActiveState, missingPullDownSupport
      )
  end io
  object io:
    export ir.constraints.IO.LevelVolt
    type dir = dir.type => ir.ConfigN[ir.constraints.IO.Dir]
    object dir:
      import ir.constraints.IO.Dir
      export Dir.IN as in
      export Dir.OUT as out
      export Dir.INOUT as inout
    type standard = standard.type => ir.ConfigN[ir.constraints.IO.Standard]
    object standard:
      import ir.constraints.IO.Standard
      export Standard.LVCMOS as lvcmos
      export Standard.LVTTL as lvttl
      export Standard.LVDS as lvds
      export Standard.SchmittTrigger as schmitttrigger
    type slewRate = slewRate.type => ir.ConfigN[ir.constraints.IO.SlewRate]
    object slewRate:
      import ir.constraints.IO.SlewRate
      export SlewRate.SLOWEST as slowest
      export SlewRate.FASTEST as fastest
      export SlewRate.CUSTOM as custom
    type pullMode = pullMode.type => ir.ConfigN[ir.constraints.IO.PullMode]
    object pullMode:
      import ir.constraints.IO.PullMode
      export PullMode.UP as up
      export PullMode.DOWN as down

  object timing:
    type InclusionPolicy = ir.ClkRstInclusionPolicy
    final val InclusionPolicy = ir.ClkRstInclusionPolicy

    type inclusionPolicy = inclusionPolicy.type => ir.ConfigN[ir.ClkRstInclusionPolicy]
    object inclusionPolicy:
      import ir.ClkRstInclusionPolicy
      export ClkRstInclusionPolicy.AsNeeded as asneeded
      export ClkRstInclusionPolicy.AlwaysAtTop as alwaysattop

    final case class ignore(
        bitIdx: ir.ConfigN[Int] = None,
        maxFreqMinPeriod: ir.ConfigN[ir.RateNumber] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Ignore =
        ir.constraints.Timing.Ignore(bitIdx, maxFreqMinPeriod)

    final case class clock(
        rate: ir.ConfigN[ir.RateNumber] = None,
        edge: clock.edge = None,
        portName: ir.ConfigN[String] = None,
        inclusionPolicy: timing.inclusionPolicy = None,
        grpName: ir.ConfigN[String] = None,
        bitIdx: ir.ConfigN[Int] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Clock =
        ir.constraints.Timing.Clock(
          rate, edge(clock.edge), portName, inclusionPolicy(timing.inclusionPolicy), grpName, bitIdx
        )
    object clock:
      type edge = edge.type => ir.ConfigN[ir.ClkCfg.Edge]
      object edge:
        import ir.ClkCfg.Edge
        export Edge.Rising as rising
        export Edge.Falling as falling
      type Rate = DFConstOf[DFTime | DFFreq]

    final case class reset(
        mode: reset.mode = None,
        active: reset.active = None,
        portName: ir.ConfigN[String] = None,
        inclusionPolicy: timing.inclusionPolicy = None,
        bitIdx: ir.ConfigN[Int] = None
    ) extends SigConstraint:
      val asIR: ir.constraints.Timing.Reset =
        ir.constraints.Timing.Reset(
          mode(reset.mode), active(reset.active), portName,
          inclusionPolicy(timing.inclusionPolicy), bitIdx
        )
    object reset:
      type mode = mode.type => ir.ConfigN[ir.RstCfg.Mode]
      object mode:
        import ir.RstCfg.Mode
        export Mode.Async as async
        export Mode.Sync as sync
      type active = active.type => ir.ConfigN[ir.RstCfg.Active]
      object active:
        import ir.RstCfg.Active
        export Active.Low as low
        export Active.High as high

    final case class related(domainContainer: RTDomainContainer)(using DFC) extends Constraint:
      val asIR: ir.constraints.Timing.Related =
        ir.constraints.Timing.Related(
          domainContainer.containedOwner.asIR
            .asInstanceOf[ir.DomainBlock | ir.DFDesignBlock]
            .refTW[ir.DomainBlock]
        )
  end timing
end constraints
