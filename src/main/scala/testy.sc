import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._


val a = DFUInt(8)
val b = a + a.prev(2)
val c = b.wc + b
c.forceOut
Almanac.getList
Almanac.printEntrees()

