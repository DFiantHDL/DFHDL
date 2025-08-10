package dfhdl.compiler.ir
import dfhdl.compiler.printing.{Printer, HasCodeString, refCodeString}
import dfhdl.internals.*
import upickle.default.*
import dfhdl.internals.StableEnum

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
  enum FlattenMode extends HWAnnotation, StableEnum derives ReadWriter:
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
    def merge(that: SigConstraint): Option[SigConstraint] =
      if (this == that) Some(this) else None
    def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint
    val bitIdx: ConfigN[Int]
  final case class DeviceID(
      vendor: DeviceID.Vendor,
      deviceName: String,
      packageName: String,
      speedGrade: String,
      deviceVersion: String
  ) extends GlobalConstraint
      derives CanEqual, ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      s"""@deviceID(${vendor.codeString}, "$deviceName", "$packageName", $speedGrade, $deviceVersion)"""
  final case class DeviceProperties(properties: Map[String, String]) extends GlobalConstraint
      derives ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val props = properties.map { case (k, v) => s""""$k" -> "$v"""" }.mkString(", ")
      s"""@deviceProperties($props)"""
  object DeviceID:
    enum Vendor extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case XilinxAMD, AlteraIntel, Lattice, Gowin
      def codeString(using Printer): String = "deviceID.Vendor." + this.toString
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
      sizeLimitMB: Int
  ) extends GlobalConstraint derives ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    def codeString(using Printer): String =
      val params = List(
        csParam("flashPartName", flashPartName),
        csParam("interface", interface),
        csParam("sizeLimitMB", sizeLimitMB)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@deviceConfig($params)"""
  end DeviceConfig
  object DeviceConfig:
    enum Interface extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case SMAPx8, SMAPx16, SMAPx32, SPIx1, SPIx2, SPIx4, SPIx8, BPIx8, BPIx16
      def codeString(using Printer): String = "deviceConfig.Interface." + this.toString

  extension [T](configN: ConfigN[T])
    def merge(that: ConfigN[T]): ConfigN[T] =
      (configN, that) match
        case (None, None)                                         => None
        case (t: T @unchecked, None)                              => t
        case (None, t: T @unchecked)                              => t
        case (t1: T @unchecked, t2: T @unchecked) if t1 equals t2 => t1
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
      levelVolt: ConfigN[IO.LevelVolt] = None,
      standard: ConfigN[IO.Standard] = None,
      slewRate: ConfigN[IO.SlewRate] = None,
      driveStrength: ConfigN[Int] = None,
      pullMode: ConfigN[IO.PullMode] = None
  ) extends SigConstraint derives CanEqual, ReadWriter:
    protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
    override def merge(that: SigConstraint): Option[SigConstraint] =
      that match
        case that: IO if bitIdx == that.bitIdx =>
          Some(
            IO(
              bitIdx = bitIdx,
              loc = loc.merge(that.loc),
              levelVolt = levelVolt.merge(that.levelVolt),
              standard = standard.merge(that.standard),
              slewRate = slewRate.merge(that.slewRate),
              driveStrength = driveStrength.merge(that.driveStrength),
              pullMode = pullMode.merge(that.pullMode)
            )
          )
        case _ => None
    def updateBitIdx(bitIdx: ConfigN[Int]): SigConstraint =
      this.copy(bitIdx = bitIdx)
    def codeString(using Printer): String =
      val params = List(
        csParam("bitIdx", bitIdx),
        csParam("loc", loc),
        csParam("levelVolt", levelVolt),
        csParam("standard", standard),
        csParam("slewRate", slewRate),
        csParam("driveStrength", driveStrength),
        csParam("pullMode", pullMode)
      ).filter(_.nonEmpty).mkString(", ")
      s"""@io($params)"""
    end codeString
  end IO
  object IO:
    type LevelVolt = 3.3 | 2.5 | 1.8 | 1.2
    enum Standard extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case LVCMOS, LVTTL, LVDS
      def codeString(using Printer): String = "io.Standard." + this.toString
      def withLevelVolt(levelVolt: LevelVolt): String =
        val num = (levelVolt * 10).toInt
        this match
          case LVCMOS => s"LVCMOS$num"
          case LVTTL  => s"LVTTL"
          case LVDS   => s"LVDS_$num"
    enum SlewRate extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case SLOW, FAST
      def codeString(using Printer): String = "io.SlewRate." + this.toString
    enum PullMode extends StableEnum, HasCodeString derives CanEqual, ReadWriter:
      case UP, DOWN
      def codeString(using Printer): String = "io.PullMode." + this.toString
  end IO

  object Timing:
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
        rate: RateNumber
    ) extends Constraint derives CanEqual, ReadWriter:
      protected def `prot_=~`(that: HWAnnotation)(using MemberGetSet): Boolean = this == that
      lazy val getRefs: List[DFRef.TwoWayAny] = Nil
      def copyWithNewRefs(using RefGen): this.type = this
      def codeString(using Printer): String =
        s"""@timing.clock(${csParam("rate", rate)})"""
  end Timing
end constraints
