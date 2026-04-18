package dfhdl.app
import dfhdl.*
import dfhdl.compiler.ir
import core.{
  DFValAny, asValAny, injectGlobalCtx, asConstOf, DFBit, DFBool, DFInt32, DFDouble, DFString,
  DFBits, DFUInt, DFSInt, DFConstOf
}
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.collection.immutable.ListMap
import scala.util.matching.Regex
import dfhdl.internals.metaContextIgnore

case class DesignArg(name: String, value: Any, desc: String)(using dfc: DFC):
  // True iff this argument carries a default value (user-written or synthetic).
  // Arguments with no default at all are filtered out of the help screen.
  val hasDefault: Boolean = DesignArg.valueIsPresent(value)

  // True iff the default value was synthesized by the compiler plugin (i.e.
  // the user did not provide one). Synthetic defaults are suppressed from the
  // `(default = ...)` annotation in the help screen.
  val isSyntheticDefault: Boolean = value match
    case dfConst: DFValAny =>
      import dfc.getSet
      dfConst.asIR.hasTagOf[ir.SyntheticDefaultTag]
    case _ => false

  val typeName: String =
    value match
      case _: String         => "String"
      case _: Int            => "Int"
      case _: Double         => "Double"
      case _: Boolean        => "Boolean"
      case _: BigInt         => "Int"
      case dfConst: DFValAny =>
        dfConst.asIR.dfType.runtimeChecked match
          case ir.DFBool    => "Boolean"
          case ir.DFBit     => "Bit"
          case ir.DFInt32   => "Int"
          case ir.DFDouble  => "Double"
          case ir.DFString  => "String"
          case _: ir.DFBits => "Bits"
          case ir.DFUInt(_) => "UInt"
          case ir.DFSInt(_) => "SInt"
          case _            => ""
      case _ => ""

  // Raw scalar value used for CLI round-trips: scallop's ValueConverter parses
  // user input into this form (Int/Double/Boolean/String) and `updateScalaValue`
  // consumes it. For Bits/UInt/SInt/Bit the raw form is the DFHDL-literal String
  // the user typed (e.g. `h"ff"`, `d"16'5"`, `sd"32'-5"`, `0`/`1`).
  def getScalaValue: Any =
    val data = value match
      case dfConst: DFValAny =>
        dfConst.asIR.dfType.runtimeChecked match
          case ir.DFBool    => dfConst.asConstOf[DFBool].toScalaBoolean
          case ir.DFInt32   => dfConst.asConstOf[DFInt32].toScalaInt
          case ir.DFDouble  => dfConst.asConstOf[DFDouble].toScalaDouble
          case ir.DFString  => dfConst.asConstOf[DFString].toScalaString
          case _            =>
            // Bit / Bits / UInt / SInt: show the DFHDL literal form the user
            // would type at the CLI.
            formatDFHDLLiteral(dfConst)
      case _ => value
    data match
      case bigInt: BigInt => bigInt.toInt
      case _              => data

  // DFHDL-literal display of the default value for help output.
  def defaultDisplay: String =
    value match
      case s: String         => s"\"$s\""
      case dfConst: DFValAny =>
        dfConst.asIR.dfType.runtimeChecked match
          case ir.DFString =>
            s"\"${dfConst.asConstOf[DFString].toScalaString}\""
          case _ => formatDFHDLLiteral(dfConst)
      case other => other.toString

  private def formatDFHDLLiteral(dfConst: DFValAny): String =
    import dfc.getSet
    given Printer = DefaultPrinter
    val ir.DFVal.Const(dfType, data, _, _, _) = dfConst.asIR.runtimeChecked: @unchecked
    summon[Printer].csConstData(dfType, data)

  def updateScalaValue(updatedScalaValue: Any): DesignArg =
    // If the CLI-supplied value equals the current default, keep the existing
    // `DesignArg` as-is. This preserves the original const — including any
    // tags (e.g. `SyntheticDefaultTag`) — which would otherwise be lost by
    // constructing a fresh const below.
    if (getScalaValue.equals(updatedScalaValue)) this
    else
      val updatedValue: Any = value match
        case dfConst: DFValAny =>
          import dfc.getSet
          val dfType = dfConst.dfType
          dfConst.asIR.dfType.runtimeChecked match
            case ir.DFBool =>
              val b = updatedScalaValue match
                case b: Boolean => b
                case s: String  => parseBool(s)
                case other      => parseBool(other.toString)
              core.DFVal.Const.forced(dfType, Some(b))
            case ir.DFInt32 =>
              val bi = updatedScalaValue match
                case i: Int    => BigInt(i)
                case s: String => BigInt(s.trim)
                case other     => BigInt(other.toString)
              core.DFVal.Const.forced(dfType, Some(bi))
            case ir.DFDouble =>
              core.DFVal.Const.forced(dfType, Some(updatedScalaValue))
            case ir.DFString =>
              core.DFVal.Const.forced(dfType, Some(updatedScalaValue))
            case ir.DFBit =>
              val b = parseBit(updatedScalaValue.toString)
              core.DFVal.Const.forced(dfType, Some(b))
            case _: ir.DFBits =>
              parseBitsLiteral(updatedScalaValue.toString, dfConst)
            case ir.DFUInt(_) =>
              parseDecimalLiteral(updatedScalaValue.toString, dfConst, signedForced = false)
            case ir.DFSInt(_) =>
              parseDecimalLiteral(updatedScalaValue.toString, dfConst, signedForced = true)
            case _ => dfConst
        case _ =>
          updatedScalaValue match
            case i: Int if value.isInstanceOf[BigInt] => BigInt(i)
            case other                                => other
      copy(value = updatedValue)
  end updateScalaValue

  private def parseBit(s: String): Boolean = s.trim match
    case "0"   => false
    case "1"   => true
    case other =>
      throw new IllegalArgumentException(
        s"Invalid Bit literal for design argument $name: '$other' (expected '0' or '1')"
      )

  private def parseBool(s: String): Boolean = s.trim match
    case "true"  => true
    case "false" => false
    case other   =>
      throw new IllegalArgumentException(
        s"Invalid Boolean literal for design argument $name: '$other' (expected 'true' or 'false')"
      )

  // Accepts `b"..."`, `h"..."`, optionally with a `W'` width prefix inside, or
  // raw content (treated as binary when it is 0/1/? only, else hex).
  private val binHexPattern: Regex = """([bh])"(.*)"""".r
  @metaContextIgnore
  private def parseBitsLiteral(input: String, dfConst: DFValAny): DFValAny =
    import dfc.getSet
    val trimmed = input.trim
    val (op, body) = trimmed match
      case binHexPattern(op, inner) => (op, inner)
      case raw                      =>
        // Heuristic: if it's only binary-or-bubble chars, treat as binary; else hex.
        val binOnly = raw.forall(c => c == '0' || c == '1' || c == '?' || c == '_' || c == ' ')
        (if (binOnly) "b" else "h", raw)
    val currentWidth = dfConst.asIR.dfType.runtimeChecked match
      case dt: ir.DFBits => dt.widthIntOpt.getOrElse(
          throw new IllegalArgumentException(
            s"Design argument $name has a parametric width and cannot be set from the CLI."
          )
        )
      case _ => throw new IllegalArgumentException(s"Design argument $name is not a Bits type.")
    val (payload, explicitWidth): (String, Option[Int]) = body match
      case s"$w'$rest" if w.nonEmpty && w.forall(_.isDigit) => (rest, Some(w.toInt))
      case s                                                => (s, None)
    val dataOrErr = op match
      case "b" => ir.DFBits.dataFromBinString(payload)
      case "h" => ir.DFBits.dataFromHexString(payload)
    val (valueBits, bubbleBits) = dataOrErr match
      case Right(d) => d
      case Left(m)  => throw new IllegalArgumentException(s"$name: $m")
    val targetWidth = explicitWidth.getOrElse(valueBits.length.toInt.max(currentWidth))
    if (targetWidth != currentWidth)
      throw new IllegalArgumentException(
        s"Design argument $name expects width $currentWidth bits but got width $targetWidth."
      )
    val resizedValue = valueBits.resize(currentWidth)
    val resizedBubble = bubbleBits.resize(currentWidth)
    core.DFVal.Const.forced(dfConst.dfType, (resizedValue, resizedBubble))
  end parseBitsLiteral

  private val decPattern: Regex = """(sd|d)"(.*)"""".r
  @metaContextIgnore
  private def parseDecimalLiteral(
      input: String,
      dfConst: DFValAny,
      signedForced: Boolean
  ): DFValAny =
    import dfc.getSet
    val trimmed = input.trim
    val (op, body) = trimmed match
      case decPattern(op, inner) => (op, inner)
      case raw                   => (if (signedForced) "sd" else "d", raw)
    val payload = body match
      case s"$w'$rest" if w.nonEmpty && w.forall(_.isDigit) => rest
      case s                                                => s
    val cleaned = payload.replace("_", "").replace(",", "")
    val bigInt =
      try BigInt(cleaned)
      catch
        case _: NumberFormatException =>
          throw new IllegalArgumentException(
            s"Invalid decimal literal for design argument $name: '$input'"
          )
    core.DFVal.Const.forced(dfConst.dfType, Some(bigInt))
  end parseDecimalLiteral

end DesignArg

object DesignArg:
  // Defined here (outside the DFC-bearing class) so the DFHDL plugin does not
  // rewrite the null-check through a meta-context forwarder. `Option(x)`
  // yields `None` for a null reference, so this cleanly sidesteps `!= null`
  // (which the plugin treats as a DFHDL comparison and rewrites).
  @metaContextIgnore
  private def valueIsPresent(value: Any): Boolean = Option(value).isDefined

type DesignArgs = ListMap[String, DesignArg]
object DesignArgs:
  def empty: DesignArgs = ListMap.empty
  def apply(
      argNames: List[String],
      argValues: List[Any],
      argDescs: List[String]
  )(using DFC): DesignArgs =
    ListMap.from(
      argNames.lazyZip(argValues).lazyZip(argDescs).map((name, value, desc) =>
        name -> DesignArg(name, value, desc)
      )
    )
  def apply(iter: Iterable[(String, DesignArg)]): DesignArgs = ListMap.from(iter)
end DesignArgs
