import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._


val a = DFUInt(8)

val b = a + a
val c = b + 1
b.forceOut
Almanac.getList

