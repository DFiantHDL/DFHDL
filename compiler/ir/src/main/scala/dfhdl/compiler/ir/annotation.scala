package dfhdl.compiler.ir
import dfhdl.compiler.printing.{Printer, HasCodeString, refCodeString}
import dfhdl.internals.*
import upickle.default.*

object annotation:
  sealed abstract class HWAnnotation extends HasRefCompare[HWAnnotation], Product, Serializable,
        HasCodeString derives CanEqual

  object HWAnnotation:
    given ReadWriter[HWAnnotation] = ReadWriter.merge(
      summon[ReadWriter[Unused]],
      summon[ReadWriter[Pure.type]],
      summon[ReadWriter[FlattenMode]],
      summon[ReadWriter[constraints.Constraint]]
    )

  enum Unused extends HWAnnotation derives ReadWriter:
    case Quiet, Keep, Prune
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      this match
        case Quiet => "@hw.annotation.unused.quiet"
        case Keep  => "@hw.annotation.unused.keep"
        case Prune => "@hw.annotation.unused.prune"

  case object Pure extends HWAnnotation:
    given ReadWriter[Pure.type] = macroRW
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String = "@hw.annotation.pure"

  /** Flattening Mode:
    *   - transparent: $memberName
    *   - prefix: $ownerName$sep$memberName
    *   - suffix: $memberName$sep$ownerName
    */
  enum FlattenMode extends HWAnnotation derives ReadWriter:
    case Transparent
    case Prefix(sep: String)
    case Suffix(sep: String)
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      "@hw.annotation.flattenMode." + (
        this match
          case Transparent => "transparent()"
          case Prefix(sep) => s"""prefix("$sep")"""
          case Suffix(sep) => s"""suffix("$sep")"""
      )
  object FlattenMode:
    val defaultPrefixUnderscore = FlattenMode.Prefix("_")
end annotation

object constraints:
  import annotation.HWAnnotation
  sealed abstract class Constraint extends HWAnnotation derives ReadWriter
  sealed abstract class GlobalConstraint extends Constraint derives ReadWriter
  sealed abstract class SigConstraint extends Constraint, HasTypeName derives ReadWriter:
    def merge(that: SigConstraint, withPriority: Boolean = false): Option[SigConstraint] =
      if (this == that) Some(this) else None
    def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint
    val bitIdx: ConfigN[Int]
  final case class DeviceID(
      vendor: DeviceID.Vendor,
      deviceName: String,
      partName: String,
      deviceVersion: String
  ) extends GlobalConstraint
      derives CanEqual, ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val params = List(
        csParam("vendor", vendor),
        csParam("deviceName", deviceName),
        csParam("partName", partName),
        csParam("deviceVersion", deviceVersion)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@deviceID($params)"""
  end DeviceID
  final case class DeviceInfo(
      slewRateSlowest: ConfigN[Int],
      slewRateFastest: ConfigN[Int]
  ) extends GlobalConstraint
      derives CanEqual, ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val params = List(
        csParam("slewRateSlowest", slewRateSlowest),
        csParam("slewRateFastest", slewRateFastest)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@deviceInfo($params)"""
  end DeviceInfo
  final case class DeviceProperties(properties: Map[String, String]) extends GlobalConstraint
      derives ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val props = properties.map { case (k, v) => s""""$k" -> "$v"""" }.mkString(", ")
      s"""@deviceProperties($props)"""
  object DeviceID:
    enum Vendor extends HasCodeString derives CanEqual, ReadWriter:
      case XilinxAMD, Lattice, Gowin
      case AlteraIntel(pro: Boolean)
      case TinyTapeout
      def codeString(using Printer): String = "_." + this.toString.toLowerCase
      def libName: String = this match
        case AlteraIntel(_) => "alteraintel"
        case _              => this.toString.toLowerCase
  final case class ToolOptions(options: Map[String, String]) extends GlobalConstraint
      derives ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val options = this.options.map { case (k, v) => s""""$k" -> "$v"""" }.mkString(", ")
      s"""@toolOptions($options)"""

  final case class DeviceConfig(
      flashPartName: String,
      interface: DeviceConfig.Interface,
      sizeLimitMb: Int,
      masterRate: ConfigN[RateNumber]
  ) extends GlobalConstraint derives ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val params = List(
        csParam("flashPartName", flashPartName),
        csParam("interface", interface),
        csParam("sizeLimitMb", sizeLimitMb),
        csParam("masterRate", masterRate)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@deviceConfig($params)"""
  end DeviceConfig
  object DeviceConfig:
    sealed trait Interface extends HasCodeString derives CanEqual, ReadWriter:
      val busWidth: Int
    object Interface:
      final case class MasterSMAP(busWidth: Int) extends Interface:
        def codeString(using Printer): String = s"_.mastersmap($busWidth)"
      final case class SlaveSMAP(busWidth: Int) extends Interface:
        def codeString(using Printer): String = s"_.slavesmap($busWidth)"
      final case class MasterSPI(busWidth: Int) extends Interface:
        def codeString(using Printer): String = s"_.masterspi($busWidth)"
      final case class MasterBPI(busWidth: Int) extends Interface:
        def codeString(using Printer): String = s"_.masterbpi($busWidth)"
      case object SlaveSerial extends Interface:
        val busWidth: Int = 1
        def codeString(using Printer): String = s"_.slaveserial"
      case object MasterSerial extends Interface:
        val busWidth: Int = 1
        def codeString(using Printer): String = s"_.masterserial"
    end Interface
  end DeviceConfig

  extension [T](configN: ConfigN[T])
    def merge(that: ConfigN[T], withPriority: Boolean = false): ConfigN[T] =
      (configN, that) match
        case (None, None)                                         => None
        case (t: T @unchecked, None)                              => t
        case (None, t: T @unchecked)                              => t
        case (t1: T @unchecked, t2: T @unchecked) if t1 equals t2 => t1
        case (_, t: T @unchecked) if withPriority                 => t
        case x => throw new IllegalArgumentException("Constraint merge error: " + x)
  extension (list: List[SigConstraint])
    /** Merge constraints that are of the same type and are mergeable. */
    def merge: List[SigConstraint] =
      list.foldLeft(List.empty[SigConstraint]) { (acc, cs) =>
        var merged = false
        val newAcc = acc.map { next =>
          if (merged) next
          else cs.merge(next) match
            case Some(mergedCs) =>
              merged = true
              mergedCs
            case None => next
        }
        if (merged) newAcc
        else cs :: newAcc
      }.reverse

    /** Consolidate several constraints that are all the same for all the bits into a single
      * constraint. This should be applied after merging.
      */
    def consolidate(length: Int)(using MemberGetSet): List[SigConstraint] =
      list.groupByOrdered(_.typeName).map {
        case (typeName, cs) if cs.length == length =>
          val consolidated = cs.map(_.updateBitIdx(None))
          if (consolidated.forall(_ =~ consolidated.head))
            consolidated.head :: Nil
          else cs
        case (typeName, cs) => cs
      }.flatten
  end extension

  private def csParam[T](name: String, value: ConfigN[T])(using printer: Printer): String =
    value match
      case None                            => ""
      case ref: DFRef.TwoWayAny @unchecked => s"$name = ${ref.refCodeString}"
      case cs: HasCodeString               => s"$name = ${cs.codeString}"
      case str: String                     => s"$name = \"$str\""
      case num: FreqNumber                 => s"$name = ${printer.csDFFreqData(num)}"
      case num: TimeNumber                 => s"$name = ${printer.csDFTimeData(num)}"
      case _                               => s"$name = ${value}"

  given ReadWriter[IO.LevelVolt] = summon[ReadWriter[Double]].asInstanceOf[ReadWriter[IO.LevelVolt]]
  final case class IO(
      bitIdx: ConfigN[Int] = None,
      loc: ConfigN[String] = None,
      dir: ConfigN[IO.Dir] = None,
      levelVolt: ConfigN[IO.LevelVolt] = None,
      standard: ConfigN[IO.Standard] = None,
      slewRate: ConfigN[IO.SlewRate] = None,
      driveStrength: ConfigN[Int] = None,
      pullMode: ConfigN[IO.PullMode] = None,
      dualPurposeGroups: ConfigN[String] = None,
      unusedPullMode: ConfigN[IO.PullMode] = None,
      invertActiveState: ConfigN[Boolean] = None,
      missingPullDownSupport: ConfigN[Boolean] = None
  ) extends SigConstraint derives CanEqual, ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    override def merge(that: SigConstraint, withPriority: Boolean = false): Option[SigConstraint] =
      that match
        case that: IO if bitIdx == that.bitIdx =>
          Some(
            IO(
              bitIdx = bitIdx,
              loc = loc.merge(that.loc, withPriority),
              dir = dir.merge(that.dir, withPriority),
              levelVolt = levelVolt.merge(that.levelVolt, withPriority),
              standard = standard.merge(that.standard, withPriority),
              slewRate = slewRate.merge(that.slewRate, withPriority),
              driveStrength = driveStrength.merge(that.driveStrength, withPriority),
              pullMode = pullMode.merge(that.pullMode, withPriority),
              dualPurposeGroups = dualPurposeGroups.merge(that.dualPurposeGroups, withPriority),
              unusedPullMode = unusedPullMode.merge(that.unusedPullMode, withPriority),
              invertActiveState = invertActiveState.merge(that.invertActiveState, withPriority),
              missingPullDownSupport =
                missingPullDownSupport.merge(that.missingPullDownSupport, withPriority)
            )
          )
        case _ => None
    def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint =
      this.copy(bitIdx = bitIdx)
    def codeString(using Printer): String =
      val params = List(
        csParam("bitIdx", bitIdx),
        csParam("loc", loc),
        csParam("dir", dir),
        csParam("levelVolt", levelVolt),
        csParam("standard", standard),
        csParam("slewRate", slewRate),
        csParam("driveStrength", driveStrength),
        csParam("pullMode", pullMode),
        csParam("dualPurposeGroups", dualPurposeGroups),
        csParam("unusedPullMode", unusedPullMode),
        csParam("invertActiveState", invertActiveState),
        csParam("missingPullDownSupport", missingPullDownSupport)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@io($params)"""
    end codeString
  end IO
  object IO:
    export DFVal.Modifier.Dir
    type LevelVolt = 3.3 | 3.0 | 2.5 | 1.8 | 1.5 | 1.2
    enum Standard extends HasCodeString derives CanEqual, ReadWriter:
      case LVCMOS, LVTTL, LVDS, SchmittTrigger
      def codeString(using Printer): String = "_." + this.toString.toLowerCase
      def withLevelVolt(levelVolt: LevelVolt): String =
        val num = (levelVolt * 10).toInt
        this match
          case LVCMOS         => s"LVCMOS$num"
          case LVTTL          => s"LVTTL"
          case LVDS           => s"LVDS_$num"
          case SchmittTrigger =>
            throw new IllegalArgumentException("Found unexpected use of SchmittTrigger.")
    enum SlewRate extends HasCodeString derives CanEqual, ReadWriter:
      case SLOWEST, FASTEST
      case CUSTOM(value: Int)
      def codeString(using Printer): String = "_." + this.toString.toLowerCase
    enum PullMode extends HasCodeString derives CanEqual, ReadWriter:
      case UP, DOWN
      def codeString(using Printer): String = "_." + this.toString.toLowerCase
  end IO

  object Timing:
    type InclusionPolicy = ClkRstInclusionPolicy
    final val InclusionPolicy = ClkRstInclusionPolicy

    final case class Ignore(
        bitIdx: ConfigN[Int] = None,
        maxFreqMinPeriod: ConfigN[RateNumber] = None
    ) extends SigConstraint
        derives CanEqual, ReadWriter:
      protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
      lazy val getRefs: List[DFRef.TwoWayAny] = Nil
      def copyWithNewRefs(using RefGen): this.type = this
      def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint =
        this.copy(bitIdx = bitIdx)
      def codeString(using Printer): String =
        val params = List(
          csParam("bitIdx", bitIdx),
          csParam("maxFreqMinPeriod", maxFreqMinPeriod)
        ).filter(_.nonEmpty).mkString(", ")
        s"""@timing.ignore($params)"""
    end Ignore

    final case class Clock(
        rate: ConfigN[RateNumber] = None,
        edge: ConfigN[ClkCfg.Edge] = None,
        portName: ConfigN[String] = None,
        inclusionPolicy: ConfigN[ClkRstInclusionPolicy] = None,
        grpName: ConfigN[String] = None,
        bitIdx: ConfigN[Int] = None
    ) extends SigConstraint derives CanEqual, ReadWriter:
      protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
      lazy val getRefs: List[DFRef.TwoWayAny] = Nil
      def copyWithNewRefs(using RefGen): this.type = this
      override def merge(
          that: SigConstraint,
          withPriority: Boolean = false
      ): Option[SigConstraint] =
        that match
          case that: Clock if bitIdx == that.bitIdx =>
            Some(
              Clock(
                rate = rate.merge(that.rate, withPriority),
                edge = edge.merge(that.edge, withPriority),
                portName = portName.merge(that.portName, withPriority),
                inclusionPolicy = inclusionPolicy.merge(that.inclusionPolicy, withPriority),
                grpName = grpName.merge(that.grpName, withPriority),
                bitIdx = bitIdx
              )
            )
          case _ => None
      def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint =
        this.copy(bitIdx = bitIdx)
      def codeString(using Printer): String =
        val params = List(
          csParam("rate", rate),
          csParam("edge", edge),
          csParam("portName", portName),
          csParam("inclusionPolicy", inclusionPolicy),
          csParam("grpName", grpName),
          csParam("bitIdx", bitIdx)
        ).filter(_.nonEmpty).mkString(", ")
        s"""@timing.clock($params)"""
    end Clock
    object Clock:
      export ClkCfg.Edge
      type Rate = RateNumber
      final val Rate = RateNumber

    final case class Reset(
        mode: ConfigN[RstCfg.Mode] = None,
        active: ConfigN[RstCfg.Active] = None,
        portName: ConfigN[String] = None,
        inclusionPolicy: ConfigN[ClkRstInclusionPolicy] = None,
        bitIdx: ConfigN[Int] = None
    ) extends SigConstraint derives CanEqual, ReadWriter:
      protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
      lazy val getRefs: List[DFRef.TwoWayAny] = Nil
      def copyWithNewRefs(using RefGen): this.type = this
      override def merge(
          that: SigConstraint,
          withPriority: Boolean = false
      ): Option[SigConstraint] =
        that match
          case that: Reset if bitIdx == that.bitIdx =>
            Some(
              Reset(
                mode = mode.merge(that.mode, withPriority),
                active = active.merge(that.active, withPriority),
                portName = portName.merge(that.portName, withPriority),
                inclusionPolicy = inclusionPolicy.merge(that.inclusionPolicy, withPriority),
                bitIdx = bitIdx
              )
            )
          case _ => None
      def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint =
        this.copy(bitIdx = bitIdx)
      def codeString(using Printer): String =
        val params = List(
          csParam("mode", mode),
          csParam("active", active),
          csParam("portName", portName),
          csParam("inclusionPolicy", inclusionPolicy),
          csParam("bitIdx", bitIdx)
        ).filter(_.nonEmpty).mkString(", ")
        s"""@timing.reset($params)"""
    end Reset
    object Reset:
      export RstCfg.{Mode, Active}

    final case class Related(ref: Related.Ref) extends Constraint derives CanEqual, ReadWriter:
      protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean =
        that match
          case Related(thatRef) => ref =~ thatRef
          case _                => false
      lazy val getRefs: List[DFRef.TwoWayAny] = List(ref)
      def copyWithNewRefs(using RefGen): this.type =
        Related(ref.copyAsNewRef).asInstanceOf[this.type]
      def codeString(using Printer): String =
        s"""@timing.related(${ref.refCodeString})"""
    end Related
    object Related:
      type Ref = DFRef.TwoWay[DomainBlock | DFDesignBlock, DomainBlock]
  end Timing
end constraints
