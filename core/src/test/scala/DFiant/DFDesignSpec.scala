import DFiant.*
import DFiant.internals.AllowTopLevel
import munit.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ID(using DFC) extends DFDesign:
  val x = DFSInt(16) <> IN
  val y = DFSInt(16) <> OUT
  y := x

class IDTop(using DFC) extends DFDesign:
  val x   = DFSInt(16) <> IN
  val y   = DFSInt(16) <> OUT
  val id1 = new ID
  val id2 = new ID
  id1.x <> x
  id1.y <> id2.x
  id2.y <> y

class IDTopVia(using DFC) extends DFDesign:
  self =>
  val x     = DFSInt(16) <> IN
  val y     = DFSInt(16) <> OUT
  val id1_x = DFSInt(16) <> IN
  val id1_y = DFSInt(16) <> OUT
  val id2_x = DFSInt(16) <> IN
  val id2_y = DFSInt(16) <> OUT
  val id1 = new ID:
    this.x <> id1_x
    this.y <> id1_y
  val id2 = new ID:
    this.x <> id2_x
    this.y <> id2_y
  x     <> id1_x
  id1_y <> id2_x
  y     <> id2_y
end IDTopVia

class DFDesignSpec extends FunSuite, AllowTopLevel:
  val id = new IDTopVia
  id.printCodeString()

end DFDesignSpec
