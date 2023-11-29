package ArithSpec
import dfhdl.*
import munit.*
import lib.arith.prioEnc

class PrioEncSpec extends DFSpec:
  test("Printing Encoder-8"):
    assertCodeString(
      """|val b32 = Bits(32) <> VAR
         |val res = prioEncRecur(b32)
         |""".stripMargin
    ) {
      val b32 = Bits(32) <> VAR
      val res = prioEnc(b32)
      res.width.verifyInlined(6)
    }
end PrioEncSpec
