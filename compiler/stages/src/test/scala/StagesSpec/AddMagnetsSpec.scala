package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.addMagnets
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class AddMagnetsSpec extends StageSpec:
  case class M1() extends Magnet(Bit)
  case class M2() extends Magnet(Bit)
  test("Basic design with magnet as in and out") {
    class Top extends EDDesign:
      val x = M1 <> IN
      val y = M1 <> OUT
    val top = (new Top).addMagnets
    assertCodeString(
      top,
      """|case class M1() extends Magnet(Bit)
         |
         |class Top extends EDDesign:
         |  val x = M1 <> IN
         |  val y = M1 <> OUT
         |end Top
         |""".stripMargin
    )
  }
  test("Basic hierarchy with bottom-up and top-down magnet propagation") {
    class Deeper extends EDDesign:
      val u8 = UInt(8) <> IN
      val x  = M1      <> IN
      val y  = M2      <> OUT
      process(all):
        y.actual :== x.actual
    class Deep extends EDDesign:
      val u8     = UInt(8) <> IN
      val deeper = Deeper()
      deeper.u8 <> u8
    class Inside extends EDDesign:
      val u8   = UInt(8) <> IN
      val deep = Deep()
      deep.u8 <> u8
    class Top extends EDDesign:
      val u8     = UInt(8) <> IN
      val x      = M1      <> IN
      val y      = M2      <> OUT
      val inside = Inside()
      inside.u8 <> u8
    val top = (new Top).addMagnets
    assertCodeString(
      top,
      """|case class M1() extends Magnet(Bit)
         |case class M2() extends Magnet(Bit)
         |
         |class Deeper extends EDDesign:
         |  val u8 = UInt(8) <> IN
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  process(all):
         |    y.actual :== x.actual
         |end Deeper
         |
         |class Deep extends EDDesign:
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  val u8 = UInt(8) <> IN
         |  val deeper = Deeper()
         |  deeper.u8 <> u8
         |end Deep
         |
         |class Inside extends EDDesign:
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  val u8 = UInt(8) <> IN
         |  val deep = Deep()
         |  deep.u8 <> u8
         |end Inside
         |
         |class Top extends EDDesign:
         |  val u8 = UInt(8) <> IN
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  val inside = Inside()
         |  inside.u8 <> u8
         |end Top
         |""".stripMargin
    )
  }
  test("Basic hierarchy with bottom-up and THEN top-down magnet propagation") {
    class Deeper1 extends EDDesign:
      val u8 = UInt(8) <> IN
      val x  = M1      <> IN
      val y  = M2      <> OUT
      process(all):
        y.actual :== 0
    class Deeper2 extends EDDesign:
      val u8 = UInt(8) <> IN
      val x  = M1      <> OUT
      val y  = M2      <> IN
      process(all):
        x.actual :== 1
    class Deep1 extends EDDesign:
      val u8     = UInt(8) <> IN
      val deeper = Deeper1()
      deeper.u8 <> u8
    class Deep2 extends EDDesign:
      val u8     = UInt(8) <> IN
      val deeper = Deeper2()
      deeper.u8 <> u8
    class Inside1 extends EDDesign:
      val u8   = UInt(8) <> IN
      val deep = Deep1()
      deep.u8 <> u8
    class Inside2 extends EDDesign:
      val u8   = UInt(8) <> IN
      val deep = Deep2()
      deep.u8 <> u8
    class Top extends EDDesign:
      val u8      = UInt(8) <> IN
      val inside1 = Inside1()
      val inside2 = Inside2()
      inside1.u8 <> u8
      inside2.u8 <> u8
    val top = (new Top).addMagnets
    assertCodeString(
      top,
      """|case class M1() extends Magnet(Bit)
         |case class M2() extends Magnet(Bit)
         |
         |class Deeper1 extends EDDesign:
         |  val u8 = UInt(8) <> IN
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  process(all):
         |    y.actual :== 0
         |end Deeper1
         |
         |class Deep1 extends EDDesign:
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  val u8 = UInt(8) <> IN
         |  val deeper = Deeper1()
         |  deeper.u8 <> u8
         |end Deep1
         |
         |class Inside1 extends EDDesign:
         |  val x = M1 <> IN
         |  val y = M2 <> OUT
         |  val u8 = UInt(8) <> IN
         |  val deep = Deep1()
         |  deep.u8 <> u8
         |end Inside1
         |
         |class Deeper2 extends EDDesign:
         |  val u8 = UInt(8) <> IN
         |  val x = M1 <> OUT
         |  val y = M2 <> IN
         |  process(all):
         |    x.actual :== 1
         |end Deeper2
         |
         |class Deep2 extends EDDesign:
         |  val x = M1 <> OUT
         |  val y = M2 <> IN
         |  val u8 = UInt(8) <> IN
         |  val deeper = Deeper2()
         |  deeper.u8 <> u8
         |end Deep2
         |
         |class Inside2 extends EDDesign:
         |  val x = M1 <> OUT
         |  val y = M2 <> IN
         |  val u8 = UInt(8) <> IN
         |  val deep = Deep2()
         |  deep.u8 <> u8
         |end Inside2
         |
         |class Top extends EDDesign:
         |  val u8 = UInt(8) <> IN
         |  val inside1 = Inside1()
         |  val inside2 = Inside2()
         |  inside1.u8 <> u8
         |  inside2.u8 <> u8
         |end Top""".stripMargin
    )
  }
end AddMagnetsSpec
