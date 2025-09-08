package dfhdl.compiler.ir
import dfhdl.internals.BitVector
import upickle.default.*
import scala.collection.mutable.ArrayBuffer
opaque type Data = Any
object Data:
  given Conversion[Any, Data] = identity
  given ReadWriter[BitVector] = readwriter[String].bimap(
    _.toBin,
    BitVector.fromBin(_).get
  )
  given CanEqual[Any, Any] = CanEqual.derived
  val vectorDataWriter = summon[ReadWriter[Vector[Data]]]
  val listDataWriter = summon[ReadWriter[List[Data]]]
  given ReadWriter[Data] = readwriter[ujson.Value].bimap(
    data =>
      data match
        case None                                          => ujson.Null
        case (valueBits: BitVector, bubbleBits: BitVector) =>
          writeJs(("bits", (valueBits, bubbleBits)))
        case Some(decimalOrEnumValue: BigInt) => writeJs(("decimal", decimalOrEnumValue))
        case Some(bitOrBoolValue: Boolean)    => writeJs(("bool", bitOrBoolValue))
        case Some(doubleValue: Double)        => writeJs(("double", doubleValue))
        case Some(stringValue: String)        => writeJs(("string", stringValue))
        case time: TimeNumber                 => writeJs(("time", (time.value, time.unit)))
        case freq: FreqNumber                 => writeJs(("freq", (freq.value, freq.unit)))
        case number: LiteralNumber            => writeJs(("number", number.value))
        case vectorData: Vector[Data]         =>
          given ReadWriter[Vector[Data]] = vectorDataWriter
          writeJs(("vector", vectorData))
        case listData: List[Data] =>
          given ReadWriter[List[Data]] = listDataWriter
          writeJs(("list", listData))
    ,
    json =>
      json match
        case ujson.Null                                           => None
        case ujson.Arr(ArrayBuffer(ujson.Str("bits"), bitsValue)) =>
          read[(BitVector, BitVector)](bitsValue)
        case ujson.Arr(ArrayBuffer(ujson.Str("decimal"), decimalValue)) =>
          Some(read[BigInt](decimalValue))
        case ujson.Arr(ArrayBuffer(ujson.Str("bool"), boolValue)) =>
          Some(read[Boolean](boolValue))
        case ujson.Arr(ArrayBuffer(ujson.Str("double"), doubleValue)) =>
          Some(read[Double](doubleValue))
        case ujson.Arr(ArrayBuffer(ujson.Str("string"), stringValue)) =>
          Some(read[String](stringValue))
        case ujson.Arr(ArrayBuffer(ujson.Str("time"), timeValue)) =>
          val (value, unit) = read[(BigDecimal, TimeNumber.Unit)](timeValue)
          TimeNumber(value, unit)
        case ujson.Arr(ArrayBuffer(ujson.Str("freq"), freqValue)) =>
          val (value, unit) = read[(BigDecimal, FreqNumber.Unit)](freqValue)
          FreqNumber(value, unit)
        case ujson.Arr(ArrayBuffer(ujson.Str("number"), numberValue)) =>
          read[LiteralNumber](numberValue)
        case ujson.Arr(ArrayBuffer(ujson.Str("vector"), vectorData)) =>
          given ReadWriter[Vector[Data]] = vectorDataWriter
          read[Vector[Data]](vectorData)
        case ujson.Arr(ArrayBuffer(ujson.Str("list"), listData)) =>
          given ReadWriter[List[Data]] = listDataWriter
          read[List[Data]](listData)
        case d => throw new Exception(s"Invalid data: $d")
  )
end Data
