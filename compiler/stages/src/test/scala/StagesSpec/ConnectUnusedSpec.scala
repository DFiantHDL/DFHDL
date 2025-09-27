package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.connectUnused
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ConnectUnusedSpec extends StageSpec:
  test("Connect unused ports to OPEN for internal design instances") {
    class SubDesign extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.annotation.unused.keep()
      val unused_out = SInt(8) <> OUT
      unused_out := 0
      y          := x

    class TopDesign extends DFDesign:
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val sub = new SubDesign()
      sub.x <> x
      y     <> sub.y

    val design = (new TopDesign).connectUnused
    assertCodeString(
      design,
      """|class SubDesign extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @hw.annotation.unused.keep
         |  val unused_out = SInt(8) <> OUT
         |  unused_out := sd"8'0"
         |  y := x
         |end SubDesign
         |
         |class TopDesign extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val sub = SubDesign()
         |  sub.unused_out <> OPEN
         |  sub.x <> x
         |  y <> sub.y
         |end TopDesign
         |""".stripMargin
    )
  }

  test("Multiple internal design instances with unused ports") {
    class SubDesign extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.annotation.unused.keep()
      val unused_out = SInt(8) <> OUT
      unused_out := 0
      y          := x

    class TopDesign extends DFDesign:
      val x    = SInt(16) <> IN
      val y    = SInt(16) <> OUT
      val sub1 = new SubDesign()
      val sub2 = new SubDesign()
      sub1.x <> x
      sub2.x <> sub1.y
      y      <> sub2.y

    val design = (new TopDesign).connectUnused
    assertCodeString(
      design,
      """|class SubDesign extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @hw.annotation.unused.keep
         |  val unused_out = SInt(8) <> OUT
         |  unused_out := sd"8'0"
         |  y := x
         |end SubDesign
         |
         |class TopDesign extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val sub1 = SubDesign()
         |  sub1.unused_out <> OPEN
         |  val sub2 = SubDesign()
         |  sub2.unused_out <> OPEN
         |  sub1.x <> x
         |  sub2.x <> sub1.y
         |  y <> sub2.y
         |end TopDesign
         |""".stripMargin
    )
  }

  test("Top design unused ports are not affected") {
    class TopDesign extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      @hw.annotation.unused.keep()
      val unused_out = SInt(8) <> OUT
      unused_out := 0
      y          := x

    val design = (new TopDesign).connectUnused
    assertCodeString(
      design,
      """|class TopDesign extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  @hw.annotation.unused.keep
         |  val unused_out = SInt(8) <> OUT
         |  unused_out := sd"8'0"
         |  y := x
         |end TopDesign
         |""".stripMargin
    )
  }
end ConnectUnusedSpec
