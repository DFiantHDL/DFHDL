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
  test("for loop inside if") {
    class Foo() extends RTDesign:
      val z      = Bit         <> IN
      val x      = UInt(8) X 7 <> VAR.REG
      val FooStr = "Hello World!".toByteVector
      if (z)
        for (i <- 0 until 7)
          x(i).din := FooStr(i)

    val foo = (new Foo).simpleOrder
    assertCodeString(
      foo,
      """|class Foo extends RTDesign:
         |  val FooStr: Bits[8] X 12 <> CONST = DFVector(Bits(8) X 12)(h"48", h"65", h"6c", h"6c", h"6f", h"20", h"57", h"6f", h"72", h"6c", h"64", h"21")
         |  val z = Bit <> IN
         |  val x = UInt(8) X 7 <> VAR.REG
         |  if (z)
         |    for (i <- 0 until 7)
         |      x(i).din := FooStr(i).uint
         |    end for
         |  end if
         |end Foo""".stripMargin
    )
  }
end OrderMembersSpec
