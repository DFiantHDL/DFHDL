import DFiant._
import scodec.bits._
import GlobalDesign._

val b8 = DFBits(8)

val b4 = b8.bits(3,0)

val u4 = b4.toDFUInt

val a = u4 + u4
a.wc




