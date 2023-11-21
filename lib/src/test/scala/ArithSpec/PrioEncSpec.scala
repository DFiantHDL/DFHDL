package ArithSpec
import dfhdl.*
import munit.*
import lib.arith.prioEnc

class PrioEncSpec extends DFSpec:
  test("Printing Encoder-8"):
    // TODO: this needs to be fixed. The name should propagate through the inline method prioEnc
    assertCodeString(
      """|val b8 = Bits(8) <> VAR
         |val ret = prioEncRecur(b8)
         |""".stripMargin
    ) {
      val b8 = Bits(8) <> VAR
      val res = prioEnc(b8)

    }
end PrioEncSpec
