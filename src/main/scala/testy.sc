import DFiant._
import singleton.ops._
import scodec.bits._
import DFiant.internals._

implicit val dsn = GlobalDesign
val u8 = DFUInt(8)
val u2 = DFUInt(8)

val a = u8 * u2
a.wc