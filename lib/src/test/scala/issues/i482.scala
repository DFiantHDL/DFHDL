package issues.i482

import dfhdl.*

// The declaration half of the use-site `class top` collision (the pure declaration-site case is
// `i458`). The instantiating sibling lives in `i482_wrapper.scala` ON PURPOSE: a package member
// referenced from ANOTHER compilation unit ranks BELOW a wildcard import, so only a cross-file
// reference reproduces the bug. Keeping both in one file would make this fixture pass either way.
class top(
    val WIDTH: Int <> CONST = 8
) extends EDDesign:
  val din = Bits(WIDTH) <> IN
  val dout = Bits(WIDTH) <> OUT
  dout <> din
end top
