package ArithSpec
import dfhdl.*
import munit.*
import lib.arith.prioEnc

class PrioEncSpec extends DesignSpec:
  test("Printing Encoder-32"):
    object Test:
      @top(false) class PrioTest extends DFDesign:
        val b32 = Bits(32) <> VAR
        val res = prioEnc(b32)
        res._2.verifyWidth(5)

    import Test.*
    PrioTest().assertCodeString(
      """|@hw.pure
         |def prioEncRecur_0(value: Bits[2] <> VAL): (Bit, Bits[1]) <> DFRET =
         |  (value(1) || value(0), value(1, 1))
         |end prioEncRecur_0
         |
         |@hw.pure
         |def prioEncRecur_1(value: Bits[4] <> VAL): (Bit, Bits[2]) <> DFRET =
         |  val lsPrio = prioEncRecur_0(value(1, 0))
         |  val msPrio = prioEncRecur_0(value(3, 2))
         |  val selPrio: Bits[1] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_1
         |
         |@hw.pure
         |def prioEncRecur_2(value: Bits[8] <> VAL): (Bit, Bits[3]) <> DFRET =
         |  val lsPrio = prioEncRecur_1(value(3, 0))
         |  val msPrio = prioEncRecur_1(value(7, 4))
         |  val selPrio: Bits[2] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_2
         |
         |@hw.pure
         |def prioEncRecur_3(value: Bits[16] <> VAL): (Bit, Bits[4]) <> DFRET =
         |  val lsPrio = prioEncRecur_2(value(7, 0))
         |  val msPrio = prioEncRecur_2(value(15, 8))
         |  val selPrio: Bits[3] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_3
         |
         |@hw.pure
         |def prioEncRecur_4(value: Bits[32] <> VAL): (Bit, Bits[5]) <> DFRET =
         |  val lsPrio = prioEncRecur_3(value(15, 0))
         |  val msPrio = prioEncRecur_3(value(31, 16))
         |  val selPrio: Bits[4] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_4
         |
         |class PrioTest extends DFDesign:
         |  val b32 = Bits(32) <> VAR
         |  val res = prioEncRecur_4(b32)
         |end PrioTest
         |""".stripMargin
    )
  test("Printing Encoder-31"):
    object Test:
      @top(false) class PrioTest extends DFDesign:
        val b31 = Bits(31) <> VAR
        val res = prioEnc(b31)
        res._2.verifyWidth(5)

    import Test.*
    PrioTest().assertCodeString(
      """|@hw.pure
         |def prioEncRecur_0(value: Bits[2] <> VAL): (Bit, Bits[1]) <> DFRET =
         |  (value(1) || value(0), value(1, 1))
         |end prioEncRecur_0
         |
         |@hw.pure
         |def prioEncRecur_1(value: Bits[4] <> VAL): (Bit, Bits[2]) <> DFRET =
         |  val lsPrio = prioEncRecur_0(value(1, 0))
         |  val msPrio = prioEncRecur_0(value(3, 2))
         |  val selPrio: Bits[1] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_1
         |
         |@hw.pure
         |def prioEncRecur_2(value: Bits[8] <> VAL): (Bit, Bits[3]) <> DFRET =
         |  val lsPrio = prioEncRecur_1(value(3, 0))
         |  val msPrio = prioEncRecur_1(value(7, 4))
         |  val selPrio: Bits[2] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_2
         |
         |@hw.pure
         |def prioEncRecur_3(value: Bits[16] <> VAL): (Bit, Bits[4]) <> DFRET =
         |  val lsPrio = prioEncRecur_2(value(7, 0))
         |  val msPrio = prioEncRecur_2(value(15, 8))
         |  val selPrio: Bits[3] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_3
         |
         |@hw.pure
         |def prioEncRecur_4(value: Bits[31] <> VAL): (Bit, Bits[5]) <> DFRET =
         |  val lsPrio = prioEncRecur_3(value(14, 0).resize(16))
         |  val msPrio = prioEncRecur_3(value(30, 15))
         |  val selPrio: Bits[4] <> VAL =
         |    if (msPrio._1) msPrio._2
         |    else lsPrio._2
         |  (msPrio._1 || lsPrio._1, (msPrio._1.bits, selPrio).toBits)
         |end prioEncRecur_4
         |
         |class PrioTest extends DFDesign:
         |  val b31 = Bits(31) <> VAR
         |  val res = prioEncRecur_4(b31)
         |end PrioTest
         |""".stripMargin
    )
end PrioEncSpec
