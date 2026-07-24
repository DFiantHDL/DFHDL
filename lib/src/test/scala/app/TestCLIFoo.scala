package app
import dfhdl.*

// Fixture used by `DesignArgsCLISpec`. The DFHDL plugin injects a `main` into
// this design's companion object; the spec invokes that `main` reflectively
// since the injected member is not visible at typer time.
class TestCLIFoo(
    val a: Int <> CONST,
    val b: Boolean <> CONST = true,
    val c: Bit <> CONST,
    val d: String <> CONST,
    val e: Double <> CONST,
    val f: Bits[8] <> CONST,
    val g: UInt[16] <> CONST,
    val h: SInt[32] <> CONST
) extends EDDesign
