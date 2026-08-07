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
end TypePrinterSpec
