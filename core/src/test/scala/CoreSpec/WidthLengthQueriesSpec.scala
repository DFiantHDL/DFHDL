package CoreSpec
import dfhdl.*
import dfhdl.compiler.printing.DefaultPrinter

// `.width` applied on a DFType is the `$clog2` width-derivation idiom (issue #457): construct
// the type with `.until`/`.to` and recover the width the constructor computed. The recovered
// width of a parametric type stays symbolic (`clog2(N)` below). `.length` equals `.width` for
// the bit-accurate scalars (`Bits`/`UInt`/`SInt`) and counts ELEMENTS for vectors, on both
// DFTypes and values. Both queries return `Int <> CONST`, and a `val` binding a query keeps its
// name in the generated code: a pre-existing (parametric) width constant is rebound through a
// named Ident (`toDFConstQuery`), never restamped (issue #449).
class WidthLengthQueriesSpec extends NoDFCSpec:
  // the freshly elaborated design, before any of the stages that rename and reorder members
  private def codeString(dsn: core.Design): String =
    val db = dsn.getDB
    DefaultPrinter(using db.getSet).csDB

  test("type and value width/length queries") {
    class Top extends EDDesign:
      val N: Int <> CONST = 854
      val ADDR_WIDTH = UInt.until(N).width
      val a = UInt(ADDR_WIDTH) <> OUT
      val W8 = Bits(8).width
      val LB = Bits(8).length
      val LU = UInt(8).length
      val LS = SInt(8).length
      val TVW = (Bits(8) X 4).width
      val TVL = (Bits(8) X 4).length
      val o = UInt(8) <> OUT
      val OL = o.length
      o <> OL.bits.uint.resize(8)
      a <> 0
    assertNoDiff(
      codeString(Top()),
      """|class Top extends EDDesign:
         |  val N: Int <> CONST = 854
         |  val ADDR_WIDTH: Int <> CONST = clog2(N)
         |  val a = UInt(ADDR_WIDTH) <> OUT
         |  val W8: Int <> CONST = 8
         |  val LB: Int <> CONST = 8
         |  val LU: Int <> CONST = 8
         |  val LS: Int <> CONST = 8
         |  val TVW: Int <> CONST = 32
         |  val TVL: Int <> CONST = 4
         |  val o = UInt(8) <> OUT
         |  val OL: Int <> CONST = 8
         |  o <> OL.bits.uint.resize(8)
         |  a <> d"1'0".resize(ADDR_WIDTH)
         |end Top""".stripMargin
    )
  }
end WidthLengthQueriesSpec
