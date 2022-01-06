import DFiant.*
class Example(using DFC) extends DFDesign:
  val i = DFUInt(3) <> IN
  // 8 elements vector of 10-bit unsigned int
  val v = DFUInt(10).X(8) <> VAR init all(0)
  // assign to specific element
  v(0) := 22
  // mux selection assignment
  v(i) := 70
  // assign vector: 0, 10, 20, ..., 70
  v := Vector.tabulate(8)(i => i * 10)
  // 4x4 matrix of 8-bit unsigned int
  val v44 = DFUInt(8).X(4).X(4) <> VAR init all(all(0))
  v44(3)(1) := 22
