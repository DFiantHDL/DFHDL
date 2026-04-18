package app
import dfhdl.*

// Fixture used by `DesignArgsCLISpec`. The DFHDL plugin synthesizes a
// `top_TestCLIFoo` DFApp object from the `@top` annotation; the spec invokes
// its `main` reflectively since the generated object is not visible at typer
// time.
@top class TestCLIFoo(
    val a: Int <> CONST,
    val b: Boolean <> CONST = true,
    val c: Bit <> CONST,
    val d: String <> CONST,
    val e: Double <> CONST,
    val f: Bits[8] <> CONST,
    val g: UInt[16] <> CONST,
    val h: SInt[32] <> CONST
) extends EDDesign
