import DFiant._
//import singleton.ops._
import tokens._
import scodec.bits._
import DFiant.internals._




val a = DFUInt(8).init(5)
val b = a + a
b.getInit.codeString
val c = b.wc + b
val d = c.bits().toDFUInt.getInit

//val e = d.toDFUInt
//d.forceOut
//Almanac.getList
//Almanac.printEntrees()

