import scodec.bits._
implicit class BitVectorExtras(vec : BitVector) {
  def lzc : Long = {
    val l = for (i <- 0L until vec.length if vec(i)) yield i
    if (l.isEmpty) vec.length else l.head
  }
  def lengthOfValue : Long = if (lzc == vec.length) 1L else vec.length - lzc
  def toLength(newLength : Long) : BitVector = {
    if (newLength > vec.length) vec.padLeft(newLength)
    else if (newLength < vec.length) vec.drop(vec.length - newLength)
    else vec
  }
  def revIdx(bitIdx : Long) : Long = vec.length - 1 - bitIdx //reverse index for BitVector
  def bit(idx : Long) : Boolean = vec(revIdx(idx))
  def bits(hiIdx : Long, loIdx : Long) : BitVector = {
    val riLoIdx = revIdx(hiIdx)
    val riHiLow = revIdx(loIdx)
    vec.slice(riLoIdx, riHiLow + 1)
  }
  def padToMulsOf(bitsNum : Int) : BitVector = {
    val paddedVecLength = ((vec.length + bitsNum - 1) / bitsNum) * bitsNum
    vec.padLeft(paddedVecLength)
  }
  def normalize : BitVector = {
    val nibble = 4
    val lov = lengthOfValue
    //narrowing the vector by removing all the leftest zeros
    val narrowVec = vec.takeRight(lov)
    //default printing of bitvectors is padding-right in `toHex`.
    //padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    narrowVec.padToMulsOf(nibble)
  }
  def toShortString : String = {
    val nibble = 4
    val lov = lengthOfValue
    //narrowing the vector by removing all the leftest zeros
    val narrowVec = vec.takeRight(lov)
    //default printing of bitvectors is padding-right in `toHex`.
    //padding left is much more intuitive for us because we consider
    // the leftest presented bit to be to MSbit.
    s"0x${normalize.toHex}"
  }
  def toBigInt : BigInt = {
    val len = vec.length
    val ext = vec.padLeft(len + 1)
    BigInt(ext.padToMulsOf(8).toByteArray)
  }
}

bin"010000001".toBigInt