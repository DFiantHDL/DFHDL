import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._

val one = BitVector.fromInt(2002)
one(0)
one(7)

TokenUInt(32, 2002)

TokenBits(32, 2002).toUInt

