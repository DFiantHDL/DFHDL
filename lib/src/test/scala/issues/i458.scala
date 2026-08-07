package issues.i458

import dfhdl.*

// A design class literally named `top` (the common Verilog convention for the top-level
// module). The class must carry NO explicit annotation: the auto-`@top` injection is the
// path under test. An injection spelled with an unqualified `top` resolves to this very
// class and fails compilation with "Cyclic reference involving class top", so the plugin
// must inject the fully qualified `@_root_.dfhdl.top` instead.
class top extends EDDesign:
  val a = Bit <> IN
  val b = Bit <> IN
  val y = Bit <> OUT
  y <> a && b
