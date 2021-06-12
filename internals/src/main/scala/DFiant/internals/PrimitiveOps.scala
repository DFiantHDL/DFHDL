package DFiant.internals

extension (value: BigInt)
  def asUnsigned(ofWidth: Int): BigInt = {
    if (value >= 0) value
    else {
      BigInt(2).pow(ofWidth) + value
    }
  }
  def asUnsigned: BigInt = asUnsigned(value.bitsWidth(false))
  def toBitVector(width: Int): BitVector = {
    val vec = BitVector(value.toByteArray)
    if (value < 0 && vec.length < width)
      BitVector.fill(width - vec.length)(true) ++ vec
    else vec.resize(width)
  }

extension (value: BigInt.type)
  //get the maximum BigInt given a bits width
  def maxUnsignedFromWidth(width: Int): BigInt = BigInt(2).pow(width) - 1
  def maxSignedFromWidth(width: Int): BigInt = BigInt(2).pow(width - 1) - 1
  def minSignedFromWidth(width: Int): BigInt = -BigInt(2).pow(width - 1)

extension (value: Int)
  def toBitVector(width: Int): BitVector = BigInt(value).toBitVector(width)
  def toPaddedString(maxValue: Int): String =
    s"%0${maxValue.toString.length}d".format(value)

extension (value: Boolean)
  def toBitVector(width: Int): BitVector = BitVector.fill(width)(value)
