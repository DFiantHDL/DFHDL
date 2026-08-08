package CoreSpec
import dfhdl.*
import munit.*

/** Covers the DFHDL type printer the compiler plugin installs (`DFHDLTypePrinter`), which names
  * DFHDL types the way a DFHDL user writes them in the diagnostics the compiler produces on its own
  * initiative, where `ShowType` never gets a say.
  *
  * The assertions go through `assertPluginError` rather than `assertCompileError`.
  * `assertCompileError` is built on `compiletime.testing.typeCheckErrors`, which packs its
  * diagnostics through `Message.message`; that renders under a context whose printer the compiler
  * pins to its own `Message.Printer`, so no installed printer is ever consulted.
  * `assertPluginError` renders them through `Message.toString`, which is the path the real run
  * takes (`CustomReporter` re-renders every reported diagnostic that way) and therefore the only
  * one that shows what a user actually reads. See devdocs/plugin-error-testing.md.
  */
class TypePrinterSpec extends DFSpec:
  private def foundRequiredInt(dfhdlType: String): String =
    s"""|Found:    (Foo.this.x : $dfhdlType)
        |Required: Int""".stripMargin

  test("boolean and bit"):
    assertPluginError(foundRequiredInt("Bit <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Bit <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Boolean <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Boolean <> VAR
        val e: Int = x
      """
    )

  test("bit vector widths"):
    assertPluginError(foundRequiredInt("Bits[8] <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Bits(8) <> VAR
        val e: Int = x
      """
    )
    // a non-literal width collapses to `Int` where the value enters the width algebra
    // (`IntParam.fromValue`), so a parameter-constructed width prints as `Int`
    assertPluginError(foundRequiredInt("Bits[Int] <> VAR"))(
      """
      class Foo(val WIDTH: Int <> CONST = 8) extends DFDesign:
        val x = Bits(WIDTH) <> VAR
        val e: Int = x
      """
    )
    // an explicitly written singleton width is kept, and is named after the parameter it
    // refers to
    assertPluginError(foundRequiredInt("Bits[WIDTH] <> VAR"))(
      """
      class Foo(val WIDTH: Int <> CONST = 8) extends DFDesign:
        val x = Bits[WIDTH.type] <> VAR
        val e: Int = x
      """
    )
    // a computed width names no value, so it prints as an unbounded `Int`
    assertPluginError(foundRequiredInt("Bits[Int] <> VAR"))(
      """
      class Foo(val WIDTH: Int <> CONST = 8) extends DFDesign:
        val x = Bits(WIDTH + 1) <> VAR
        val e: Int = x
      """
    )

  test("integers"):
    assertPluginError(foundRequiredInt("UInt[8] <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("SInt[8] <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = SInt(8) <> VAR
        val e: Int = x
      """
    )
    // the native (wildcard) decimal, whose width its value determines
    assertPluginError(foundRequiredInt("Int <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Int <> VAR
        val e: Int = x
      """
    )

  test("fixed point"):
    assertPluginError(foundRequiredInt("UFix[8, 4] <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = UFix(8, 4) <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("SFix[8, 4] <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = SFix(8, 4) <> VAR
        val e: Int = x
      """
    )

  test("host scalars"):
    assertPluginError(foundRequiredInt("Double <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Double <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("String <> CONST"))(
      """
      class Foo extends DFDesign:
        val x: String <> CONST = "abc"
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Unit <> VAL"))(
      """
      class Foo extends DFDesign:
        val b = Bit <> VAR
        val x: Unit <> VAL = !b
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Time <> CONST"))(
      """
      class Foo extends DFDesign:
        val x = 1.ns
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Freq <> CONST"))(
      """
      class Foo extends DFDesign:
        val x = 1.MHz
        val e: Int = x
      """
    )

  test("composites"):
    assertPluginError(foundRequiredInt("(Bits[8] <> VAL, Bit <> VAL) <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = (Bits(8), Bit) <> VAR
        val e: Int = x
      """
    )
    // a struct over named fields, an enumeration and an opaque type are all named after the
    // Scala type that declares them
    assertPluginError(foundRequiredInt("MyFields <> VAR"))(
      """
      case class MyFields(a: Bits[8] <> VAL, b: Bit <> VAL) extends Struct
      class Foo extends DFDesign:
        val x = MyFields <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("MyEnum <> VAR"))(
      """
      enum MyEnum extends Encoded:
        case A, B, C
      class Foo extends DFDesign:
        val x = MyEnum <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("MyOpq <> VAR"))(
      """
      case class MyOpq() extends Opaque(UInt(8))
      class Foo extends DFDesign:
        val x = MyOpq <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("((Bits[8] <> VAL, Bit <> VAL) X 4) <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = (Bits(8), Bit) X 4 <> VAR
        val e: Int = x
      """
    )

  test("modifiers"):
    // a port is named by its direction, exactly as `ShowType` names it. Naming it by what it
    // grants instead (an input as a readable `VAL`) rendered a reduce-over-port-slices
    // mismatch with `Bits[Int] <> VAL` on BOTH sides of the error (issue #455).
    assertPluginError(foundRequiredInt("Bits[8] <> IN"))(
      """
      class Foo extends DFDesign:
        val x = Bits(8) <> IN
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Bits[8] <> OUT"))(
      """
      class Foo extends DFDesign:
        val x = Bits(8) <> OUT
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("Bits[8] <> CONST"))(
      """
      class Foo extends DFDesign:
        val x: Bits[8] <> CONST = all(0)
        val e: Int = x
      """
    )

  test("operator precedence"):
    // `X` binds looser than `<>`, so a vector on the left of `<>` is parenthesized; a chain of
    // `X` is left-associative and needs none of its own
    assertPluginError(foundRequiredInt("(Bit X 4) <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Bit X 4 <> VAR
        val e: Int = x
      """
    )
    assertPluginError(foundRequiredInt("(Bits[8] X 4 X 2) <> VAR"))(
      """
      class Foo extends DFDesign:
        val x = Bits(8) X 4 X 2 <> VAR
        val e: Int = x
      """
    )

  test("expected types"):
    assertPluginError(
      """|Found:    ("hello" : String)
         |Required: Bits[8] <> VAL""".stripMargin
    )(
      """
      class Foo extends DFDesign:
        val e: Bits[8] <> VAL = "hello"
      """
    )
    assertPluginError(
      """|Found:    (5 : Int)
         |Required: (Bit X 4) <> VAL""".stripMargin
    )(
      """
      class Foo extends DFDesign:
        val e: (Bit X 4) <> VAL = 5
      """
    )

  test("types the printer cannot name keep their standard rendering"):
    // `DFValAny` wraps the IR's base dataflow type, which has no user-facing name; collapsing
    // it into something like ShowType's catch-all `DFType` would lose more than it gains
    assertPluginError(foundRequiredInt("dfhdl.core.DFValAny"))(
      """
      class Foo extends DFDesign:
        val x: core.DFValAny = Bit <> VAR
        val e: Int = x
      """
    )

  test("not-a-member errors keep only their core sentence"):
    // in a DFHDL compilation the compiler's own selection-error addenda mislead rather than
    // help (the import-suggestion machinery proposes DFHDL's internal conversions for every
    // receiver, and the extension-attempt transcript restates the receiver in raw types), so
    // the rewriter reduces the error to its core sentence; a tried-but-failed extension is
    // kept as a bare parenthetical
    assertSinglePluginError("value mem is not a member of UInt[8] <> OUT")(
      """
      class Foo extends EDDesign:
        val o = UInt(8) <> OUT
        o.mem
      """
    )
    // `Bit` supports neither the vector `length` (element count) nor the `Bits`/`UInt`/`SInt`
    // `length` (bit count), so the tried extensions fail
    assertSinglePluginError(
      "value length is not a member of Bit <> VAR (extension method tried)"
    )(
      """
      class Foo extends EDDesign:
        val b = Bit <> VAR
        b.length
      """
    )
    // the reduction applies to any receiver, not just DFHDL values: the suggested-import noise
    // is compilation-wide once DFHDL's conversions are on the classpath
    assertSinglePluginError("value zzz is not a member of Int")(
      """
      class Foo extends EDDesign:
        val x: Int = 1
        x.zzz
      """
    )
    // the did-you-mean hint is recomputed after the strip: upstream computes it only when no
    // other addendum exists, and the conversions make the import-suggestion addendum non-empty
    // for every selection, so without the recomputation DFHDL users would never see it
    assertSinglePluginError("value toStrig is not a member of Int - did you mean x.toString?")(
      """
      class Foo extends EDDesign:
        val x: Int = 1
        x.toStrig
      """
    )

  test("reduce over declaration slices guide rail"):
    // The issue #455 shape: `reduce` commits its type parameter to the port-modified slice
    // element type, which no operation result can conform to. The rewriter identifies the
    // enclosing fold-family call from the parse tree and spells the pinned-type remedy with
    // the actual element type. The single-error assertion also pins the diagnostic dedup:
    // the typer re-raises this mismatch through the inline expansion of `++`, once with a
    // corrupt macro-splice position, and all re-raises must collapse into this one message.
    assertSinglePluginError(
      """|Found:    Bits[Int] <> VAL
         |Required: Bits[Int] <> IN
         |
         |Note: `reduce` inferred its type parameter from the declaration (port or
         |variable) slice elements, so the operator must land back on the declaration
         |type, and an operation result is a plain value that never can. Set the type
         |parameter to the plain value type explicitly:
         |
         |  .reduce[Bits[Int] <> VAL](...)""".stripMargin
    )(
      """
      class Foo(val LANE: Int <> CONST = 3, val LANES: Int <> CONST = 3) extends EDDesign:
        val data = Bits(LANE * LANES) <> IN
        val out = Bits(LANE * LANES) <> OUT
        val list = for (i <- 0 until LANES) yield data.lsbitsAt(i * LANE, LANE)
        out <> list.reduce(_ ++ _)
      """
    )
end TypePrinterSpec
