package issues.i482

import dfhdl.*

// The use half of the `class top` collision. While the built-in annotation lived at `dfhdl.top`,
// the wildcard import above outranked the sibling `top` declared in `i482.scala` (a package member
// from another compilation unit is the LOWEST-precedence binding, below a wildcard import), so
// `new top(WIDTH = 8)` was checked against the annotation's own constructor and reported
// "dfhdl.top does not have a parameter WIDTH" without ever naming what it had resolved to. The
// annotation now lives at `dfhdl.hw.annotation.top`, so `import dfhdl.*` binds no `top` at all.
class Wrapper extends EDDesign:
  val din = Bits(8) <> IN
  val dout = Bits(8) <> OUT
  val u_top = new top(WIDTH = 8)
  u_top.din <> din
  u_top.dout <> dout
end Wrapper
