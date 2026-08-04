package CoreSpec
import dfhdl.*
import dfhdl.compiler.printing.DefaultPrinter

// A member reached through a qualifier that is not a stable path (`src(0).o`, rather than `src(0)`
// bound to a `val` first) is written with a reference type whose type arguments the compiler
// re-derives on demand, inventing a fresh skolem for the unstable prefix on every derivation.
// Operators that hand an operand to a type argument, or take an operand's type apart in an
// `inline` match, then read two different skolems for one value and fail to typecheck. This spec
// pins the operators that do either, and they work off `retyped` for exactly this reason.
//
// One shape is out of reach from the library: `sel` with BOTH branches reached through unstable
// paths fails while the compiler infers `sel`'s own type parameters, i.e. before anything in the
// method body runs. Binding the qualifier to a `val` is the workaround.
// See https://github.com/DFiantHDL/DFHDL/issues/427
class UnstablePathSpec extends NoDFCSpec:
  class Bar extends RTDesign:
    val WIDTH: Int <> CONST = 8
    val i = UInt(WIDTH) <> IN
    val o = UInt(WIDTH) <> OUT
    o := i

  // the freshly elaborated design, before any of the stages that rename and reorder members
  private def codeString(dsn: core.Design): String =
    val db = dsn.getDB
    DefaultPrinter(using db.getSet).csDB

  test("connection through an unstable path") {
    class Top extends RTDesign:
      val src = List.fill(1)(Bar())
      val dst = List.fill(1)(Bar())
      val i = UInt(8) <> IN
      val o = UInt(8) <> OUT
      src(0).i <> i // unstable prefix on the LHS
      dst(0).i <> src(0).o // unstable prefix on both sides
      o <> dst(0).o // unstable prefix on the RHS
    assertNoDiff(
      codeString(Top()),
      """|class Bar extends RTDesign:
         |  val WIDTH: Int <> CONST = 8
         |  val i = UInt(WIDTH) <> IN
         |  val o = UInt(WIDTH) <> OUT
         |  o := i
         |end Bar
         |
         |class Top extends RTDesign:
         |  val src = Bar()
         |  val dst = Bar()
         |  val i = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  val src_WIDTH: Int <> CONST = 8
         |  src.i <> i
         |  val dst_WIDTH: Int <> CONST = 8
         |  dst.i <> src.o
         |  o <> dst.o
         |end Top""".stripMargin
    )
  }

  test("comparison and selection through an unstable path") {
    class Top extends RTDesign:
      val cmp = List.fill(1)(Bar())
      val alt = List.fill(1)(Bar())
      val alt0 = alt(0) // `sel` needs one of its two branches on a stable path
      val i = UInt(8) <> IN
      val cond = Bit <> IN
      val gt = Bit <> OUT
      val o = UInt(8) <> OUT
      cmp(0).i <> i
      alt(0).i <> i
      gt := cmp(0).o > cmp(0).i // both operands through unstable prefixes
      o <> cond.sel(cmp(0).o, alt0.o)
    assertNoDiff(
      codeString(Top()),
      """|class Bar extends RTDesign:
         |  val WIDTH: Int <> CONST = 8
         |  val i = UInt(WIDTH) <> IN
         |  val o = UInt(WIDTH) <> OUT
         |  o := i
         |end Bar
         |
         |class Top extends RTDesign:
         |  val cmp = Bar()
         |  val alt = Bar()
         |  val i = UInt(8) <> IN
         |  val cond = Bit <> IN
         |  val gt = Bit <> OUT
         |  val o = UInt(8) <> OUT
         |  val cmp_WIDTH: Int <> CONST = 8
         |  cmp.i <> i
         |  val alt_WIDTH: Int <> CONST = 8
         |  alt.i <> i
         |  gt := (cmp.o > cmp.i).bit
         |  o <> cond.sel(cmp.o, alt.o)
         |end Top""".stripMargin
    )
  }
end UnstablePathSpec
