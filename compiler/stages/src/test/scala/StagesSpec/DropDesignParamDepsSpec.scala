package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDesignParamDeps
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropDesignParamDepsSpec extends StageSpec:
  test("Design parameter depending on another design parameter is inlined"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val depth: Int <> CONST = width + 2
      val x                   = Bits(width) <> IN
      val y                   = Bits(depth) <> OUT
      y <> x.resize(depth)
    end Foo
    val top = (new Foo).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 10
         |) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(depth) <> OUT
         |  y <> x.resize(depth)
         |end Foo
         |""".stripMargin
    )

  test("Multiple design parameter dependencies are inlined"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val depth: Int <> CONST  = width + 2
      val length: Int <> CONST = depth * 2
      val x                    = Bits(width)  <> IN
      val y                    = Bits(length) <> OUT
      y <> x.resize(length)
    end Foo
    val top = (new Foo).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 10,
         |    val length: Int <> CONST = 20
         |) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(length) <> OUT
         |  y <> x.resize(length)
         |end Foo
         |""".stripMargin
    )

  test("Stage only runs for VHDL'93"):
    given options.CompilerOptions.Backend = backends.vhdl.v2008
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val depth: Int <> CONST = width + 2
      val x                   = Bits(width) <> IN
      val y                   = Bits(depth) <> OUT
      y <> x.resize(depth)
    end Foo
    val top = (new Foo).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Foo(val width: Int <> CONST = 8) extends RTDesign:
         |  val depth: Int <> CONST = width + 2
         |  val x = Bits(width) <> IN
         |  val y = Bits(depth) <> OUT
         |  y <> x.resize(depth)
         |end Foo
         |""".stripMargin
    )

  test("Stage does not run for Verilog backends"):
    given options.CompilerOptions.Backend = backends.verilog.v2001
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val depth: Int <> CONST = width + 2
      val x                   = Bits(width) <> IN
      val y                   = Bits(depth) <> OUT
      y <> x.resize(depth)
    end Foo
    val top = (new Foo).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Foo(val width: Int <> CONST = 8) extends RTDesign:
         |  val depth: Int <> CONST = width + 2
         |  val x = Bits(width) <> IN
         |  val y = Bits(depth) <> OUT
         |  y <> x.resize(depth)
         |end Foo
         |""".stripMargin
    )

  test("Nested design parameter dependencies are inlined"):
    // Note: the `inner_depth` parameter is printed because the (invisible) port selector for `inner.y` is referencing it.
    // In later stages (namely the Via-Connection stage) this parameter will be visibly used for the `inner_y` variable.
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Inner(val width: Int <> CONST) extends RTDesign:
      val depth: Int <> CONST = width + 1
      val x                   = Bits(width) <> IN
      val y                   = Bits(depth) <> OUT
      y <> x.resize(depth)
    end Inner
    class Outer(val baseWidth: Int <> CONST = 8) extends RTDesign:
      val inner = Inner(baseWidth)
      val x     = Bits(baseWidth) <> IN
      val y     = Bits(baseWidth) <> OUT
      inner.x <> x
      y       <> inner.y.resize(baseWidth)
    end Outer
    val top = (new Outer).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Inner(
         |    val width: Int <> CONST,
         |    val depth: Int <> CONST = 9
         |) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(depth) <> OUT
         |  y <> x.resize(depth)
         |end Inner
         |
         |class Outer(val baseWidth: Int <> CONST = 8) extends RTDesign:
         |  val inner = Inner(
         |      width = baseWidth,
         |      depth = 9
         |  )
         |  val x = Bits(baseWidth) <> IN
         |  val y = Bits(baseWidth) <> OUT
         |  inner.x <> x
         |  val inner_depth: Int <> CONST = baseWidth + 1
         |  y <> inner.y.resize(baseWidth)
         |end Outer
         |""".stripMargin
    )

  test("Complex parameter expressions are correctly evaluated"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo(val width: Int <> CONST = 8) extends RTDesign:
      val depth: Int <> CONST  = width * 2
      val length: Int <> CONST = depth + width
      val total: Int <> CONST  = length * 2
      val x                    = Bits(width) <> IN
      val y                    = Bits(total) <> OUT
      y <> x.resize(total)
    end Foo
    val top = (new Foo).dropDesignParamDeps
    assertCodeString(
      top,
      """|class Foo(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 16,
         |    val length: Int <> CONST = 24,
         |    val total: Int <> CONST = 48
         |) extends RTDesign:
         |  val x = Bits(width) <> IN
         |  val y = Bits(total) <> OUT
         |  y <> x.resize(total)
         |end Foo
         |""".stripMargin
    )

end DropDesignParamDepsSpec
