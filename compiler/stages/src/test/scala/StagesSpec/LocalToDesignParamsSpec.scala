package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.localToDesignParams
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class LocalToDesignParamsSpec extends StageSpec:
  test("Local parameters used in IO are converted to design parameters"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo extends RTDesign:
      val width: Int <> CONST = 8
      val x                   = Bits(width)     <> IN
      val y                   = Bits(width)     <> OUT
      val x2                  = Bits(width + 1) <> IN
      val y2                  = Bits(width + 1) <> OUT
      y  <> x
      y2 <> x2
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|class Foo(val width: Int <> CONST = 8) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(width) <> OUT
         |  val x2 = Bits(width + 1) <> IN
         |  val y2 = Bits(width + 1) <> OUT
         |  y <> x
         |  y2 <> x2
         |end Foo
         |""".stripMargin
    )

  test("Local parameters used in nested designs are converted"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Inner(val width: Int <> CONST) extends RTDesign:
      val x = Bits(width) <> IN
      val y = Bits(width) <> OUT
      y <> x
    end Inner
    class Outer extends RTDesign:
      val width: Int <> CONST = 8
      val inner               = Inner(width)
      val x                   = Bits(width) <> IN
      val y                   = Bits(width) <> OUT
      inner.x <> x
      y       <> inner.y
    end Outer
    val top = (new Outer).localToDesignParams
    assertCodeString(
      top,
      """|class Inner(val width: Int <> CONST) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(width) <> OUT
         |  y <> x
         |end Inner
         |
         |class Outer(val width: Int <> CONST = 8) extends RTDesign:
         |  val inner = Inner(width = width)
         |  val x = Bits(width) <> IN
         |  val y = Bits(width) <> OUT
         |  inner.x <> x
         |  y <> inner.y
         |end Outer
         |""".stripMargin
    )

  test("Stage only runs for VHDL backends"):
    given options.CompilerOptions.Backend = backends.verilog.v2001
    class Foo extends RTDesign:
      val width: Int <> CONST = 8
      val x                   = Bits(width) <> IN
      val y                   = Bits(width) <> OUT
      y <> x
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val width: Int <> CONST = 8
         |  val x = Bits(width) <> IN
         |  val y = Bits(width) <> OUT
         |  y <> x
         |end Foo
         |""".stripMargin
    )

  test("Anonymous local parameters are not converted"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo extends RTDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      y <> x
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  y <> x
         |end Foo
         |""".stripMargin
    )

  test("Global parameters are not converted"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    val globalWidth: Int <> CONST         = 8
    class Foo extends RTDesign:
      val x = Bits(globalWidth) <> IN
      val y = Bits(globalWidth) <> OUT
      y <> x
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|val globalWidth: Int <> CONST = 8
         |
         |class Foo extends RTDesign:
         |  val x = Bits(globalWidth) <> IN
         |  val y = Bits(globalWidth) <> OUT
         |  y <> x
         |end Foo
         |""".stripMargin
    )

  test("Design parameters are not converted"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val x = Bits(width) <> IN
      val y = Bits(width) <> OUT
      y <> x
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|class Foo(val width: Int <> CONST = 8) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(width) <> OUT
         |  y <> x
         |end Foo
         |""".stripMargin
    )

  test("Complex parameter expressions and different data types in IO"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo extends RTDesign:
      val width: Int <> CONST  = 8
      val depth: Int <> CONST  = 4
      val length: Int <> CONST = width + depth
      val x1                   = Bits(width)         <> IN
      val y1                   = Bits(width)         <> OUT
      val x2                   = UInt(width + depth) <> IN
      val y2                   = UInt(width + depth) <> OUT
      val x3                   = SInt(length)        <> IN
      val y3                   = SInt(length)        <> OUT
      y1 <> x1
      y2 <> x2
      y3 <> x3
    end Foo
    val top = (new Foo).localToDesignParams
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 4,
         |    val length: Int <> CONST = width + depth
         |) extends RTDesign:
         |  val x1 = Bits(width) <> IN
         |  val y1 = Bits(width) <> OUT
         |  val x2 = UInt(width + depth) <> IN
         |  val y2 = UInt(width + depth) <> OUT
         |  val x3 = SInt(length) <> IN
         |  val y3 = SInt(length) <> OUT
         |  y1 <> x1
         |  y2 <> x2
         |  y3 <> x3
         |end Foo
         |""".stripMargin
    )

end LocalToDesignParamsSpec
