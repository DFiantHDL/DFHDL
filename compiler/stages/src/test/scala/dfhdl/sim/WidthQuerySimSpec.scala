package dfhdl.sim
import dfhdl.*

class WidthQuerySub(val W: Int <> CONST = 8) extends DFDesign:
  val din = Bits(W) <> IN
  val w = Int <> OUT
  w := din.width

class WidthQueryDut(
    val W: Int <> CONST = 8,
    val N: Int <> CONST = 5
) extends DFDesign:
  val vec = Bits(W) X N <> IN
  val len = Int <> OUT
  val wid = Int <> OUT
  val subw = Int <> OUT
  val sub = WidthQuerySub(12)
  len := vec.length
  wid := vec.width
  sub.din <> all(0)
  subw := sub.w

/** `width`/`length` query FUNCs fold to constants of the argument's resolved type width on both
  * kernel tiers, including a query over a sub-design's parametric port (resolved to the
  * instance-applied width, not the declaration default).
  */
class WidthQuerySimSpec extends SimSpec:
  bothTiers("width/length queries fold to resolved constants"): tier =>
    (new WidthQueryDut).simulation { dut =>
      assertEquals(dut.len.peek, 5)
      assertEquals(dut.wid.peek, 40)
      assertEquals(dut.subw.peek, 12)
    }.withTier(tier).run()
end WidthQuerySimSpec
