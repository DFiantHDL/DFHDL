package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.simpleOrder
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class OrderMembersSpec extends StageSpec:
  test("Simple order") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val z = Bits(8)  <> VAR
      val y = SInt(16) <> OUT
      y := x
    val id = (new ID).simpleOrder
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = Bits(8) <> VAR
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
  test("Design blocks inside if") {
    class Plus1 extends DFDesign:
      val in  = SInt(16) <> IN
      val out = SInt(16) <> OUT
      out := in + 1

    class Minus1 extends DFDesign:
      val in  = SInt(16) <> IN
      val out = SInt(16) <> OUT
      out := in - 1

    class DesignWithIf extends DFDesign:
      val sel = Boolean  <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      if (sel)
        val subDesign = Plus1()
        subDesign.in <> x
        y            := subDesign.out
      else
        val subDesign = Minus1()
        subDesign.in <> x
        y            := subDesign.out

    val design = (new DesignWithIf).simpleOrder
    assertCodeString(
      design,
      """|class Plus1 extends DFDesign:
         |  val in = SInt(16) <> IN
         |  val out = SInt(16) <> OUT
         |  out := in + sd"16'1"
         |end Plus1
         |
         |class Minus1 extends DFDesign:
         |  val in = SInt(16) <> IN
         |  val out = SInt(16) <> OUT
         |  out := in - sd"16'1"
         |end Minus1
         |
         |class DesignWithIf extends DFDesign:
         |  val sel = Boolean <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  if (sel)
         |    val subDesign = Plus1()
         |    subDesign.in <> x
         |    y := subDesign.out
         |  else
         |    val subDesign = Minus1()
         |    subDesign.in <> x
         |    y := subDesign.out
         |  end if
         |end DesignWithIf""".stripMargin
    )
  }
end OrderMembersSpec
