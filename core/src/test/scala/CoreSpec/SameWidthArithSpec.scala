package CoreSpec
import dfhdl.*
import dfhdl.compiler.printing.DefaultPrinter

// A commutative arithmetic operation is as wide as its wider operand, so on two operands of the
// same parametric width its result width would be the symbolic `Max[W, W]`, a different type from
// `W` even though it stands for it. The width algebra therefore collapses every non-literal width
// to `Int` (the fold to a precise width happens only for literals), so the result of an operation
// over collapsed operands lands back on their own type. `reduce` is the shape that demands it: it
// fixes its type parameter to the element type before the operator is typed.
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

  // An unrolled accumulation over a parametric width grows its result width as a
  // left-nested `max` chain (`((16 max W) max W) max W`). `SimplifyFunc.MaxMinChainAbsorb`
  // collapses the repeated operand, so the chain stays minimal (`16 max W`). Design
  // parameters are used since they stay symbolically opaque (a local `Int <> CONST`
  // has known data and folds through `MaxMinWithOffset` instead).
  // A named `val` over a collapsed simplification result wraps it in a named Ident (never a
  // meta restamp of the underlying anonymous member; issue #449), and the simplifications see
  // THROUGH such idents: the chain absorb and the self-cancellation below only fire when
  // `M`/`E` dereference to the expressions they name.
  test("simplifications see through named intermediates (idents)") {
    class Top(val W: Int <> CONST = 11) extends DFDesign:
      val M: Int <> CONST = 16 max W max W // absorbed, so `M` idents `16 max W`
      val E: Int <> CONST = W max W // collapsed, so `E` idents `W`
      val v = Int <> VAR
      v := M max W // absorbs through the `M` ident
      v := E - W // cancels through the `E` ident
    assertNoDiff(
      codeString(Top()),
      """|class Top(val W: Int <> CONST = 11) extends DFDesign:
         |  val M: Int <> CONST = 16 max W
         |  val E: Int <> CONST = W
         |  val v = Int <> VAR
         |  v := M
         |  v := 0
         |end Top""".stripMargin
    )
  }

  test("a repeated max/min chain over a design parameter is absorbed") {
    class Top(val W: Int <> CONST = 11) extends DFDesign:
      val v = Int <> VAR
      v := 16 max W max W max W
      v := 1 min W min W
      v := W max 5 max W
    assertNoDiff(
      codeString(Top()),
      """|class Top(val W: Int <> CONST = 11) extends DFDesign:
         |  val v = Int <> VAR
         |  v := 16 max W
         |  v := 1 min W
         |  v := W max 5
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

  // The collapse happens once, where a width VALUE enters the algebra (`IntParam.fromValue`):
  // a non-literal width enters as `IntParam[Int]`, so `Bits(LANE)` is `Bits[Int]` rather than
  // `Bits[LANE.type]`, while a literal keeps its precise type. The part-select elements below
  // are therefore `Bits[Int]`-typed and the `Seq[Bits[Int] <> VAL]` ascription conforms by
  // plain subsumption, which is what lets a width-growing operator (`++`) reduce: its collapsed
  // result is the elements' own type. Without the ascription the elements keep the port's
  // modifier, which no operation result can land back on, and that is the correct error.
  // See https://github.com/DFiantHDL/DFHDL/issues/455
  test("reduce-concat over parametric port slices") {
    class Top extends EDDesign:
      val LANE: Int <> CONST = 3
      val LANES: Int <> CONST = 3
      // compile-level pins of the boundary collapse: a parametric width constructs a
      // `Bits[Int]`/`UInt[Int]`/`SInt[Int]`, a literal width stays precise
      val cb: Bits[Int] = Bits(LANE)
      val cu: UInt[Int] = UInt(LANE)
      val cs: SInt[Int] = SInt(LANE)
      val lb: Bits[8] = Bits(8)
      val data = Bits(LANE * LANES) <> IN
      val out = Bits(LANE * LANES) <> OUT
      val list: Seq[Bits[Int] <> VAL] =
        for (i <- 0 until LANES) yield data.lsbitsAt(i * LANE, LANE)
      out <> list.reduce(_ ++ _)
    assertNoDiff(
      codeString(Top()),
      """|class Top extends EDDesign:
         |  val LANE: Int <> CONST = 3
         |  val LANES: Int <> CONST = 3
         |  val data = Bits(LANE * LANES) <> IN
         |  val out = Bits(LANE * LANES) <> OUT
         |  val list = data(LANE - 1, 0)
         |  val list = data((LANE + LANE) - 1, LANE)
         |  val list = data(((2 * LANE) + LANE) - 1, 2 * LANE)
         |  out <> (list, list, list).toBits
         |end Top""".stripMargin
    )
  }
end SameWidthArithSpec
