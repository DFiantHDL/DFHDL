package CoreSpec
import dfhdl.*
import dfhdl.compiler.printing.DefaultPrinter

// A commutative arithmetic operation is as wide as its wider operand, so on two operands of the
// same parametric width its result width is the symbolic `Max[W, W]`, a different type from `W`
// even though it stands for it (the fold happens only for literal widths, where `Max` bottoms out
// in `compiletime.ops.int.Max`). `DFVal.MaxOfSameWidth` is the conversion that closes the gap, and
// `reduce` is the shape that demands it: it fixes its type parameter to the element type before
// the operator is typed, so the operator has to land back on exactly that type.
// See https://github.com/DFiantHDL/DFHDL/issues/431
class SameWidthArithSpec extends NoDFCSpec:
  // the freshly elaborated design, before any of the stages that rename and reorder members
  private def codeString(dsn: core.Design): String =
    val db = dsn.getDB
    DefaultPrinter(using db.getSet).csDB

  test("reduce over same-width values of a parametric width") {
    class Top(val BIN_WIDTH: Int <> CONST = 11) extends EDDesign:
      val din = Bits(BIN_WIDTH * 4) <> IN
      val dout = UInt(BIN_WIDTH) <> OUT
      def bin(i: Int) = din((i + 1) * BIN_WIDTH - 1, i * BIN_WIDTH).uint
      dout <> (0 until 4).map(bin).reduce(_ + _)
    assertNoDiff(
      codeString(Top()),
      """|class Top(val BIN_WIDTH: Int <> CONST = 11) extends EDDesign:
         |  val din = Bits(BIN_WIDTH * 4) <> IN
         |  val dout = UInt(BIN_WIDTH) <> OUT
         |  dout <> (din(BIN_WIDTH - 1, 0).uint + din((2 * BIN_WIDTH) - 1, BIN_WIDTH).uint + din((3 * BIN_WIDTH) - 1, 2 * BIN_WIDTH).uint + din((4 * BIN_WIDTH) - 1, 3 * BIN_WIDTH).uint)
         |end Top""".stripMargin
    )
  }

  test("a same-width sum has its operands' own type, without reduce") {
    class Top(val BIN_WIDTH: Int <> CONST = 11) extends EDDesign:
      val din = Bits(BIN_WIDTH * 2) <> IN
      val dout = UInt(BIN_WIDTH) <> OUT
      def bin(i: Int) = din((i + 1) * BIN_WIDTH - 1, i * BIN_WIDTH).uint
      // `sameType` forces the sum to conform to the type of one operand, which is exactly what
      // `reduce` requires of the operator and nothing more
      def sameType[T](a: T)(b: T): T = b
      dout <> sameType(bin(0))(bin(0) + bin(1))
    assertNoDiff(
      codeString(Top()),
      """|class Top(val BIN_WIDTH: Int <> CONST = 11) extends EDDesign:
         |  val din = Bits(BIN_WIDTH * 2) <> IN
         |  val dout = UInt(BIN_WIDTH) <> OUT
         |  dout <> (din(BIN_WIDTH - 1, 0).uint + din((2 * BIN_WIDTH) - 1, BIN_WIDTH).uint)
         |end Top""".stripMargin
    )
  }
end SameWidthArithSpec
