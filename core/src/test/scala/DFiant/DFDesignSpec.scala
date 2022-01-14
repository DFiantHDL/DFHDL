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
//  id1.x <> x
//  id1.y <> y

class DFDesignSpec extends FunSuite, AllowTopLevel:

  val id = new IDTop
  id.printCodeString()

end DFDesignSpec
