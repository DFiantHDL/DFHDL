package DFiant.internals

type BitVector = scodec.bits.BitVector
val BitVector = scodec.bits.BitVector

extension (vec: BitVector)
  def lzc: Long =
    val l = for (i <- 0L until vec.length if vec(i)) yield i
    if (l.isEmpty) vec.length else l.head
  def lengthOfValue: Long = if (lzc == vec.length) 1L else vec.length - lzc
  def resize(newLength: Int): BitVector =
    if (newLength > vec.length) vec.padLeft(newLength)
    else if (newLength < vec.length) vec.drop(vec.length - newLength)
    else vec
  def revIdx(bitIdx: Long): Long =
    vec.length - 1 - bitIdx //reverse index for BitVector
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
    val padding = if (signed) vec(0) else vec(0)
    BitVector.fill(paddedVecLength - vec.length)(padding) ++ vec
  def toHexProper: String = padToMulsOf(4, false).toHex
  def isZeros: Boolean = vec == BitVector.low(vec.length)
  def toShortString: String =
    val nibble = 4
    val lov = lengthOfValue
    //narrowing the vector by removing all the leftest zeros
    val narrowVec = vec.takeRight(lov)
    //default printing of bitvectors is padding-right in `toHex`.
    //padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    s"0x${narrowVec.padToMulsOf(nibble, false).toHex}"
  def toBigInt(signed: Boolean): BigInt =
    val len = vec.length
    val ext = BitVector.fill(1)(vec(0)) ++ vec
    BigInt(ext.padToMulsOf(8, signed).toByteArray)
end extension

extension (iter: Iterable[BitVector])
  def bitsConcat: BitVector = iter.reduce(_ ++ _)

extension (iter: Iterable[(BitVector, BitVector)])
  def bitsConcat: (BitVector, BitVector) =
    val vecs = iter.unzip
    (vecs._1.bitsConcat, vecs._2.bitsConcat)
