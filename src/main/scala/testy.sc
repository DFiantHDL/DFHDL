import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._

val one = BitVector.fromInt(2002)
one(0)
one(7)

TokenUInt(32, 20)

TokenBits(32, 20).toUInt

TokenBits(7,bin"11").toUInt


bin"11".toLength(7).toShortString


val a = DFBits(8).init(bin"11",20,Φ,15L)

a.toDFUInt

1L.toString

BigInt("2343454636565477567567").codeString
//val a = DFBits(8).init(20)
