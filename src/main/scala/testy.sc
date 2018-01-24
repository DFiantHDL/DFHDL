import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._

val one = BitVector.fromInt(2002)
one(0)
one(7)

TokenUInt(32, 20)

TokenBits(32, 20).toUInt

TokenBits(7,bin"11").toUInt


bin"000".lengthOfValue
//val a = DFBits(8).init(20)
