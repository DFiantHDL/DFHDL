package ArithSpec
import dfhdl.*
import munit.*
import lib.arith.prioEnc

class PrioEncSpec extends DFSpec:
  test("Printing Encoder-8"):
    // TODO: this needs to be fixed. The name should propagate through the inline method prioEnc
    assertCodeString(
      """|val b32 = Bits(32) <> VAR
         |val ret = prioEncRecur(b32)
         |""".stripMargin
    ) {
      val b32 = Bits(32) <> VAR
      val res = prioEnc(b32)
      res.width.verifyInlined(6)
    }
end PrioEncSpec
