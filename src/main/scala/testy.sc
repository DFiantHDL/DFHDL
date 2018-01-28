import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._


val a = DFUInt(8)

val b = a + a
val b2 = b.wc
val c = b2 + b2
c.forceOut
Almanac.getList

