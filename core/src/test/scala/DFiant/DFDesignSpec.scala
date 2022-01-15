import DFiant.*
import DFiant.internals.AllowTopLevel
import munit.*

class ID(using DFC) extends DFDesign:
  val x = DFSInt(16) <> IN
  val y = DFSInt(16) <> OUT
  y := x

class IDTop(using DFC) extends DFDesign:
  val x = DFSInt(16) <> IN
  val y = DFSInt(16) <> OUT
  val id1 = new ID
  val id2 = new ID
  id1.x <> x
  id1.y <> id2.x
  id2.y <> y

class DFDesignSpec extends FunSuite, AllowTopLevel:
  val id = new IDTop
  id.printCodeString()

end DFDesignSpec
