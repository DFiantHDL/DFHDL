package dfhdl.internals

type BitVector = scodec.bits.BitVector
val BitVector = scodec.bits.BitVector

extension (vec: BitVector)
  def lzc: Long =
    val l = for (i <- 0L until vec.length if vec(i)) yield i
    if (l.isEmpty) vec.length else l.head
  def lengthOfValue: Long = if (lzc == vec.length) 1L else vec.length - lzc
  private def extensionPad(length: Long, signed: Boolean): BitVector =
    BitVector.fill(length)(if (signed) vec(0) else false)
  def resize(newLength: Int): BitVector =
    if (newLength > vec.length) vec.padLeft(newLength)
    else if (newLength < vec.length) vec.drop(vec.length - newLength)
    else vec
  def revIdx(bitIdx: Long): Long =
    vec.length - 1 - bitIdx // reverse index for BitVector
  def bit(idx: Long): Boolean = vec(revIdx(idx))
  def bits(hiIdx: Long, loIdx: Long): BitVector =
    val riLoIdx = revIdx(hiIdx)
    val riHiLow = revIdx(loIdx)
    vec.slice(riLoIdx, riHiLow + 1)
  def bitsWL[W](relWidth: Int, loIdx: Int): BitVector =
    val hiIdx = relWidth + loIdx - 1
    bits(hiIdx, loIdx)
  def padToMulsOf(bitsNum: Int, signed: Boolean): BitVector =
    val paddedVecLength = ((vec.length + bitsNum - 1) / bitsNum) * bitsNum
    extensionPad(paddedVecLength - vec.length, signed) ++ vec
  def toHexProper: String = padToMulsOf(4, false).toHex
  def isZeros: Boolean = vec === BitVector.low(vec.length)
  def toShortString: String =
    val nibble = 4
    val lov = lengthOfValue
    // narrowing the vector by removing all the leftest zeros
    val narrowVec = vec.takeRight(lov)
    // default printing of bitvectors is padding-right in `toHex`.
    // padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    s"0x${narrowVec.padToMulsOf(nibble, false).toHex}"
  def toBigInt(signed: Boolean): BigInt =
    val len = vec.length
    val ext = extensionPad(1L, signed) ++ vec
    BigInt(ext.padToMulsOf(8, signed).toByteArray)
  def width: Int = vec.length.toInt
  def getFalseRanges: List[(Int, Int)] =
    var ranges = List.empty[(Int, Int)]
    var start = -1 // To track the start of a false range
    for (i <- vec.width - 1 to 0 by -1)
      if (!vec.bit(i))
        if (start == -1)
          // Start a new range
          start = i
      else if (start != -1)
        // End the current range
        ranges = ranges :+ (start, i + 1)
        start = -1
    // If the last range is still open, close it
    if (start != -1)
      ranges = ranges :+ (start, 0)
    ranges
  end getFalseRanges
end extension

extension (iter: Iterable[BitVector]) def bitsConcat: BitVector = iter.reduce(_ ++ _)

extension (iter: Iterable[(BitVector, BitVector)])
  def bitsConcat: (BitVector, BitVector) =
    val vecs = iter.unzip
    (vecs._1.bitsConcat, vecs._2.bitsConcat)
