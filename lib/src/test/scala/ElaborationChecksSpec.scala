import dfhdl.*
import munit.*
import java.io.File.separatorChar as S
given options.ElaborationOptions.OnError = _.Exception
class ElaborationChecksSpec extends DesignSpec:
  val currentFilePos = s"lib${S}src${S}test${S}scala${S}"
  test("ambiguous RT dependency errors"):
    class Internal1 extends EDDesign:
      val dmn1 = new RTDomain:
        val o = Bit <> OUT
        o := 1
      val dmn2 = new RTDomain:
        val o = Bit <> OUT
        o := 1
    class Internal2 extends EDDesign:
      val dmn = new RTDomain:
        val i1 = Bit <> IN
        val i2 = Bit <> IN
    object Test:
      @top(false) class Top extends EDDesign:
        val internal1 = Internal1()
        val internal2 = Internal2()
        internal1.dmn1.o <> internal2.dmn.i1
        internal1.dmn2.o <> internal2.dmn.i2
    import Test.*
    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |Found ambiguous source RT configurations for the domain:
         |Internal2.dmn
         |Sources:
         |Internal1.dmn1
         |Internal1.dmn2
         |Possible solution:
         |Either explicitly define a configuration for the domain or drive it from a single source domain.
         |""".stripMargin
    )

  test("cyclic RT dependency errors"):
    // Two distinct design classes are used so dedup keeps each canonical
    // block separate; the dependency cycle is detected between the two
    // distinct internal domains.
    class Internal1 extends EDDesign:
      val dmn = new RTDomain:
        val i = Bit <> IN
        val o = Bit <> OUT
        o := i
    class Internal2 extends EDDesign:
      val dmn = new RTDomain:
        val i = Bit <> IN
        val o = Bit <> OUT
        o := i
    object Test:
      @top(false) class Top extends EDDesign:
        val internal1 = Internal1()
        val internal2 = Internal2()
        internal1.dmn.i <> internal2.dmn.o
        internal1.dmn.o <> internal2.dmn.i
    import Test.*
    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |Circular derived RT configuration detected. Involved in the cycle:
         |Internal1.dmn
         |Internal2.dmn
         |""".stripMargin
    )

  test("domain creation in the wrong spot"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = Boolean <> IN
        if (x)
          val dmn = new RTDomain {}
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:72:25 - 72:33
          |Hierarchy: Top.dmn
          |Operation: `apply`
          |Message:   A domain can only be directly owned by a design, an interface, or another domain.
          |""".stripMargin
    )

  test("anonymous domains are forbidden"):
    object Test:
      @top(false) class Top extends RTDesign:
        new RTDomain {} setName ("someName")
        new RTDomain {}
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL name errors!
          |Unable to determine names for the members declared at the following positions:
          |${currentFilePos}ElaborationChecksSpec.scala:88:13 - 88:21
          |
          |Explanation:
          |This can happen when utilizing the meta programming power of Scala in a way that
          |DFHDL cannot infer the actual name of the member.
          |
          |Resolution:
          |To resolve this issue use `setName` when declaring the member.
          |
          |Example 1:
          |```
          |  // Scala Vector holding 4 DFHDL ports
          |  val x_vec = Vector.fill(4)(UInt(8) <> IN setName "x_vec")
          |```
          |In this example all the ports will be named "x_vec", and DFHDL will enumerate
          |them automatically to "x_vec_0", "x_vec_1", etc.
          |
          |Example 2:
          |If you wish to give the ports an explicit unique name, you can just use the power
          |of Scala, as in the following example:
          |```
          |  val x_vec = Vector.tabulate(4)(i => UInt(8) <> IN setName s"x_vec_{i + 10}")
          |```
          |This would yield the same ports, but named "x_vec_10", "x_vec_11", etc.
          |""".stripMargin
    )
  test("non-shared assign limitations"):
    object Test:
      @top(false) class Top extends EDDesign:
        val x = Bit <> OUT
        val y = Bit <> VAR
        val ok = Bit <> VAR.SHARED
        val dmn1 = new RTDomain:
          x := 1
          y := 1
          ok := 1
        val dmn2 = new RTDomain:
          x := 0
          y := 0
          ok := 0
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:131:11 - 131:17
          |Hierarchy: Top
          |LHS:       x
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.x`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:127:11 - 127:17
          |
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:132:11 - 132:17
          |Hierarchy: Top
          |LHS:       y
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.y`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:128:11 - 128:17
          |""".stripMargin
    )

  test("port declaration in the wrong spot"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = Boolean <> IN
        if (x)
          val y = Bit <> IN
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:162:19 - 162:28
          |Hierarchy: Top.y
          |Operation: `Port/Variable constructor`
          |Message:   Ports can only be directly owned by a design, a domain or an interface.
          |""".stripMargin
    )

  test("dangling ports"):
    class ID extends EDDesign:
      val x = Bits(10) <> IN
      val y = Bits(10) <> OUT
      // no connection
    object Test:
      @top(false) class IDTop extends EDDesign:
        val x = Bits(10) <> IN
        val y = Bits(10) <> OUT

        val id = ID()
        id.y <> y
    import Test.*
    assertElaborationErrors(IDTop())( // TODO: fix fullName
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:184:18 - 184:20
          |Hierarchy: IDTop.id
          |Message:   Found a dangling (unconnected) input port `x`.
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:177:15 - 177:30
          |Hierarchy: ID
          |Message:   Found a dangling (unconnected/unassigned and uninitialized) output port `y`.
          |""".stripMargin
    )

  test("anonymous port/var declarations are forbidden"):
    object Test:
      @top(false) class Top extends RTDesign:
        Bit <> IN
        Bit <> VAR setName "someName"
        Bit <> OUT init 0
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL name errors!
          |Unable to determine names for the members declared at the following positions:
          |${currentFilePos}ElaborationChecksSpec.scala:203:9 - 203:18
          |${currentFilePos}ElaborationChecksSpec.scala:205:9 - 205:26
          |
          |Explanation:
          |This can happen when utilizing the meta programming power of Scala in a way that
          |DFHDL cannot infer the actual name of the member.
          |
          |Resolution:
          |To resolve this issue use `setName` when declaring the member.
          |
          |Example 1:
          |```
          |  // Scala Vector holding 4 DFHDL ports
          |  val x_vec = Vector.fill(4)(UInt(8) <> IN setName "x_vec")
          |```
          |In this example all the ports will be named "x_vec", and DFHDL will enumerate
          |them automatically to "x_vec_0", "x_vec_1", etc.
          |
          |Example 2:
          |If you wish to give the ports an explicit unique name, you can just use the power
          |of Scala, as in the following example:
          |```
          |  val x_vec = Vector.tabulate(4)(i => UInt(8) <> IN setName s"x_vec_{i + 10}")
          |```
          |This would yield the same ports, but named "x_vec_10", "x_vec_11", etc.
          |""".stripMargin
    )
  test("wait statement errors"):
    given options.ElaborationOptions.DefaultClkCfg.Rate = 4.sec
    object Test:
      @top(false) class Top extends RTDesign:
        val x = Bit <> IN
        process:
          8.sec.wait
          1.sec.wait
          12.sec.wait
    end Test
    import Test.*
    // TODO: figure out why there is a crash when using assertElaborationErrors
    val err =
      try
        Top()
        ""
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(
      err,
      s"""|Elaboration errors found!
          |DFiant HDL wait error!
          |Position:  lib${S}src${S}test${S}scala${S}ElaborationChecksSpec.scala:245:11 - 245:21
          |Hierarchy: Top
          |Message:   Wait duration 1.sec is not exactly divisible by the clock period 4.sec.""".stripMargin
    )
  test("latch variables are forbidden under RT domains"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = Bit <> IN
        val y = Bit <> OUT
        if (x)
          y := 1
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity/assignment error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:267:17 - 267:27
          |Hierarchy: Top
          |Message:   Found a latch variable `y`. Latches are not allowed under RT domains.""".stripMargin
    )
  test("missing port location check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @io(loc = "locClk")
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        val y = Bit <> OUT
        @io(bitIdx = 0, loc = "locz0")
        @io(bitIdx = 1, loc = "locz1")
        @io(bitIdx = 2, loc = "locz2")
        @io(bitIdx = 3, loc = "locz3")
        @io(bitIdx = 14, loc = "locz14")
        @io(bitIdx = 15, loc = "locz15")
        val z = Bits(16) <> OUT
        z := all(0)
        x <> y
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |The following top device design ports or domains are missing location constraints:
          |  Top.y
          |  Top.z with bits 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
          |To Fix:
          |Add a location constraint to the ports by connecting them to a located resource or
          |by using the `@io` constraint.""".stripMargin
    )
  test("location collision check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @io(loc = "locClk")
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locz1")
        val y = Bit <> OUT
        @io(bitIdx = 0, loc = "locz0")
        @io(bitIdx = 1, loc = "locz1")
        @io(bitIdx = 2, loc = "locz2")
        @io(bitIdx = 3, loc = "locz3")
        val z = Bits(4) <> OUT
        @io(loc = "locw")
        val w = Bits(2) <> IN
        z := all(0)
        x <> y
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |The following location constraints have collisions:
          |  Top.y and Top.z(1) are both assigned to location `locz1`
          |  Top.w has mutliple bits assigned to location `locw`
          |To Fix:
          |Ensure each location is used by a single port bit.""".stripMargin
    )
  test("clock missing timing constraint check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locy")
        val y = Bit <> OUT
        y <> x.reg(1, init = 0)
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL domain clock rate error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:341:7 - 347:32
          |Hierarchy: Top
          |Message:   Missing clock rate timing constraint.
          |To Fix:
          |Connect a 50.MHz clock resource to the domain to match your configuration.""".stripMargin
    )
  test("clock location missing check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @timing.clock(rate = 20.MHz)
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locy")
        val y = Bit <> OUT
        y <> x.reg(1, init = 0)
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |The following top device design ports or domains are missing location constraints:
          |  Top is missing a clock location constraint
          |To Fix:
          |Add a location constraint to the ports by connecting them to a located resource or
          |by using the `@io` constraint.""".stripMargin
    )
  test("clock location collision check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @timing.clock(rate = 20.MHz)
      @io(loc = "locx")
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locy")
        val y = Bit <> OUT
        y <> x.reg(1, init = 0)
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |The following location constraints have collisions:
          |  Top and Top.x are both assigned to location `locx`
          |To Fix:
          |Ensure each location is used by a single port bit.""".stripMargin
    )
  test("big input small output connection"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = UInt(8) <> IN
        val y = UInt(7) <> OUT
        x <> y
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:406:9 - 406:15
          |Hierarchy: Top
          |LHS:       x
          |RHS:       y.eby(1)
          |Message:   Unexpected write access to the immutable value y.eby(1).""".stripMargin
    )
  test("no need for clock location constraint check in internal designs"):
    object Test:
      import hw.constraints.*
      class Internal extends RTDesign:
        val x = Bit <> IN
        val y = Bit <> OUT
        y <> x.reg(1, init = 0)
      end Internal
      @deviceID(_.xilinxamd, "test", "test", "")
      @timing.clock(rate = 20.MHz)
      @io(loc = "locClk")
      @top(false) class Top extends RTDesign:
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locy")
        val y = Bit <> OUT
        val internal = Internal()
        internal.x <> x
        internal.y <> y
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      "No error found"
    )
  test("no need for clock location constraint check in internal domains"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @timing.clock(rate = 20.MHz)
      @top(false) class Top extends RTDesign:
        @io(loc = "locClk")
        val clk = Clk <> IN
        @io(loc = "locx")
        val x = Bit <> IN
        @io(loc = "locy")
        val y = Bit <> OUT
        @timing.clock(rate = 20.MHz)
        val dmn = new RTDomain:
          val clk = Clk <> VAR
        dmn.clk <> clk.as(dmn.Clk)
        y <> x.reg(1, init = 0)
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      "No error found"
    )
  test("domain constraint check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @top(false) class Top extends EDDesign:
        @io(loc = "locClk")
        @timing.clock(rate = 20.MHz)
        val dmn1 = new RTDomain:
          @io(loc = "locx")
          val x = Bit <> IN
          @io(loc = "locy")
          val y = Bit <> OUT
          y <> x.reg(1, init = 0)
        end dmn1
        @timing.clock(rate = 20.MHz)
        val dmn2 = new RTDomain:
          val x = Bit <> IN
          val y = Bit <> OUT
          y <> x.reg(1, init = 0)
        end dmn2
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |The following top device design ports or domains are missing location constraints:
         |  Top.dmn2 is missing a clock location constraint
         |  Top.dmn2.x
         |  Top.dmn2.y
         |To Fix:
         |Add a location constraint to the ports by connecting them to a located resource or
         |by using the `@io` constraint.""".stripMargin
    )
  test("clk/rst in related domain check"):
    object Test:
      @top(false) class Top extends RTDesign:
        self =>
        @hw.constraints.timing.related(self)
        val dmn = new RTDomain:
          val clk = Clk <> IN
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:505:21 - 505:30
          |Hierarchy: Top.clk
          |Operation: `Port/Variable constructor`
          |Message:   Cannot create a clk/rst in a related domain.
          |You can create the clk/rst in the primary domain `Top` and reference it here instead.""".stripMargin
    )
  test("resource direction mismatch check"):
    object Test:
      import hw.constraints.*
      @deviceID(_.xilinxamd, "test", "test", "")
      @timing.clock(rate = 20.MHz)
      @top(false) class Top extends RTDesign:
        @io(loc = "locClk")
        val clk = Clk <> IN
        @io(loc = "locx", dir = _.out)
        val x = Bit <> IN
        @io(loc = "locy", dir = _.in)
        val y = Bit <> OUT
        y <> x.reg(1, init = 0)
      end Top
    end Test
    import Test.*
    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |The following top device design ports have resource direction mismatches:
         |  Top.x direction (IN) has a resource direction (OUT) mismatch.
         |  Top.y direction (OUT) has a resource direction (IN) mismatch.
         |To Fix:
         |Make sure you connect the resource to the port with the correct direction.""".stripMargin
    )
  test("DFDecimal parameter width checks"):
    object Test:
      @top(false) class Foo(
          val WIDTH1: Int <> CONST = 8,
          val WIDTH2: Int <> CONST = 5
      ) extends EDDesign:
        val x = UInt(WIDTH1) <> OUT init h"${WIDTH2}'0"
        val y = UInt(WIDTH1) <> OUT init h"${WIDTH1 + 2}'0"
        val z = UInt(WIDTH2) <> OUT init h"${WIDTH2 - 1}'0"
      end Foo
    import Test.*
    assertElaborationErrors(Foo())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:547:42 - 547:56
          |Hierarchy: Foo
          |Operation: `apply`
          |Message:   The applied RHS value width (WIDTH2) is undefined compared to the LHS variable width (WIDTH1).
          |
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:548:42 - 548:60
          |Hierarchy: Foo
          |Operation: `apply`
          |Message:   The applied RHS value width (WIDTH1 + 2) is larger than the LHS variable width (WIDTH1).""".stripMargin
    )
  test("DFBits parameter width checks"):
    object Test:
      @top(false) class Foo(
          val WIDTH1: Int <> CONST = 8,
          val WIDTH2: Int <> CONST = 5
      ) extends EDDesign:
        val x = Bits(WIDTH1) <> OUT init h"${WIDTH2}'0"
        val y = Bits(WIDTH1) <> OUT
        val z = Bits(WIDTH2) <> OUT
        val w = y == z
      end Foo
    import Test.*
    assertElaborationErrors(Foo())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:572:42 - 572:56
          |Hierarchy: Foo
          |Operation: `apply`
          |Message:   The argument width (WIDTH2) is different than the receiver width (WIDTH1).
          |Consider applying `.resize` to resolve this issue.
          |
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:575:17 - 575:23
          |Hierarchy: Foo.w
          |Operation: `apply`
          |Message:   Cannot apply this operation between a value of WIDTH1 bits width (LHS) and a value of WIDTH2 bits width (RHS).
          |An explicit conversion must be applied.""".stripMargin
    )

  test("the same bit connected more than once check"):
    object Test:
      @top(false) class MultiConn extends EDDesign:
        val y = Bits(4) <> OUT
        y(0) <> 1
        y(0) <> 0
      end MultiConn
    import Test.*
    assertElaborationErrors(MultiConn())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:600:9 - 600:18
          |Hierarchy: MultiConn
          |LHS:       y(0)
          |RHS:       0
          |Message:   Found multiple connections write to the same variable/port `MultiConn.y`.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:599:9 - 599:18""".stripMargin
    )

  test("the same bit assigned and connected check"):
    object Test:
      @top(false) class AssignConn extends RTDesign:
        val y = Bits(4) <> OUT
        y(1, 0) := b"00"
        y(0) <> 1
      end AssignConn
    import Test.*
    assertElaborationErrors(AssignConn())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:619:9 - 619:18
          |Hierarchy: AssignConn
          |LHS:       y(0)
          |RHS:       1
          |Message:   Found multiple connections write to the same variable/port `AssignConn.y`.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:618:9 - 618:25""".stripMargin
    )
  // `wait` inside an `initial` block used to be caught here, at elaboration. The scope lattice
  // rejects it at COMPILE time now (`Initial` is a `Sequence`, deliberately not a `TimedSequence`,
  // so it has no `HasWait`), so it cannot appear in this design at all. The compile-time rejection
  // is covered in `ScopeChecksSpec`; `DB.initialCheck` keeps its elaboration check as the backstop
  // for evidence laundered through a helper `def`.
  test("initial block content errors under RT domain"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = SInt(16) <> IN
        val y = SInt(16) <> OUT.REG
        initial:
          y.din := x
          println("bad")
        y.din := x
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:643:11 - 643:21
          |Hierarchy: Top
          |Message:   An `initial` block under a register-transfer (RT) domain may only assign constant values.
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:644:11 - 644:25
          |Hierarchy: Top
          |Message:   Text output statements are not allowed inside an `initial` block under a register-transfer (RT) domain.""".stripMargin
    )

  test("initial block conflict errors"):
    object Test:
      @top(false) class Top extends EDDesign:
        val a = SInt(16) <> VAR init 0
        val b = SInt(16) <> VAR
        val c = SInt(16) <> OUT
        initial:
          a := 1
          b := 0
        initial:
          b := 1
        process(all):
          c :== a + b
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:666:11 - 666:17
          |Hierarchy: Top
          |Message:   The declaration `a` has an `init` value and is also assigned inside an `initial` block. These are mutually exclusive.
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:669:11 - 669:17
          |Hierarchy: Top
          |Message:   The declaration `b` is assigned inside more than one `initial` block.""".stripMargin
    )
  test("initial block non-constant conditional errors under RT domain"):
    object Test:
      @top(false) class Top extends RTDesign:
        val x = Bit <> IN
        val s = SInt(16) <> IN
        val y = SInt(16) <> OUT.REG
        initial:
          if (x) y.din := 0
          s match
            case 1 => y.din := 1
            case _ => y.din := 2
        y.din := 3
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:691:15 - 691:28
          |Hierarchy: Top
          |Message:   A conditional guard inside an `initial` block under a register-transfer (RT) domain must be a constant.
          |DFiant HDL initial block error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:691:28 - 694:33
          |Hierarchy: Top
          |Message:   A `match` selector inside an `initial` block under a register-transfer (RT) domain must be a constant.""".stripMargin
    )
  test("named register DIN read"):
    object Test:
      @top(false) class Top extends RTDesign:
        val r = UInt(8) <> VAR.REG init 0
        val d = r.din
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:712:17 - 712:22
          |Hierarchy: Top.d
          |Operation: `.din`
          |Message:   Cannot name a register DIN read.
          |Reading `.din` yields the register's pending value at the position of the read, so binding it
          |to a Scala `val` would hold a live view and not the snapshot it appears to be.
          |To Fix: apply `.din` directly where it is read. E.g.:
          |* Instead of `val d = x.din` followed by `y := d + 1` write `y := x.din + 1`.
          |""".stripMargin
    )
  // Scala allows referencing a class member before its definition inside the class body and
  // silently yields `null` for it, so a forward reference reaches DFHDL as a `null` value/type.
  test("forward referenced value in a connection"):
    class Sub extends RTDesign:
      val i = Bit <> IN
      val o = Bit <> OUT
      o := i
    object Test:
      @top(false) class Top extends RTDesign:
        val i = Bit <> IN
        val o = Bit <> OUT
        val sub1 = Sub()
        sub1.i <> i
        sub1.o <> fwdIn
        val sub2 = Sub()
        val fwdIn = sub2.i
        sub2.i <> sub1.o
        o <> sub2.o
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:740:9 - 740:24
          |Hierarchy: Top
          |Operation: `<>`
          |Message:   Found a reference to an uninitialized DFHDL value.
          |This is caused by a forward reference: the value is declared later in the class body.
          |To Fix:
          |Move the declaration before its first use.
          |""".stripMargin
    )
  test("forward referenced value in an assignment"):
    object Test:
      @top(false) class Top extends RTDesign:
        val i = Bit <> IN
        val o = Bit <> OUT
        o := fwd
        val fwd = Bit <> VAR
        fwd := i
        o := fwd
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:763:9 - 763:17
          |Hierarchy: Top
          |Operation: `:=`
          |Message:   Found a reference to an uninitialized DFHDL value.
          |This is caused by a forward reference: the value is declared later in the class body.
          |To Fix:
          |Move the declaration before its first use.
          |""".stripMargin
    )
  test("forward referenced dfhdl type"):
    object Test:
      @top(false) class Top extends RTDesign:
        val i = Bit <> IN
        val o = Bit <> OUT
        val bad = MyType <> VAR
        val MyType = Bits(8)
        o := i
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:785:19 - 785:32
          |Hierarchy: Top.bad
          |Operation: `Port/Variable constructor`
          |Message:   Found a reference to an uninitialized DFHDL type.
          |This is caused by a forward reference: the type is declared later in the class body.
          |To Fix:
          |Move the declaration before its first use.
          |""".stripMargin
    )
  // a method may be forward referenced (it is a Scala `def`), but a value its body captures
  // must still be declared above the call site that elaborates the body. Naming the error also
  // exercises a method design block whose instance cache was never set, because it aborted.
  test("forward referenced value captured by a method body"):
    object Test:
      @top(false) class Top extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        y <> addK(a)
        def addK(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + k
        val k: UInt[8] <> CONST = 5
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:809:14 - 809:21
          |Hierarchy: Top.addK
          |Operation: `designFromDefImpl`
          |Message:   Found a reference to an uninitialized DFHDL value.
          |This is caused by a forward reference: the value is declared later in the class body.
          |To Fix:
          |Move the declaration before its first use.
          |""".stripMargin
    )
  // An ED domain body is a concurrent scope, so a conditional expression branch in it is not a
  // block and cannot hold a named value. Such a value would be driven by a connection, which the
  // backend prints as an `assign` inside the `always_comb` the expression is wrapped into.
  test("named values inside concurrent conditional expression branches"):
    object Test:
      @top(false) class Top extends EDDesign:
        val c = Bit <> IN
        val i = UInt(8) <> IN
        val o = UInt(8) <> OUT
        o <> (
          if (c) i + 1
          else
            val inv = ~i.bits
            inv.uint
        )
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL conditional expression error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:837:23 - 837:30
          |Hierarchy: Top
          |Message:   Found the named value `inv` inside a conditional expression branch.
          |An event-driven (ED) domain body is a concurrent scope, so a conditional expression
          |branch in it is not a block and cannot hold a named value declaration.
          |To Fix:
          |Move the declaration before the conditional expression, place the conditional
          |expression inside a `process`, or turn it into a conditional statement that assigns
          |or connects its result.""".stripMargin
    )
  // In a sequential scope the branch lowers to a procedural block, so the named value becomes a
  // plain blocking assignment inside it. Legal in an ED process, in an RT domain, and in a
  // conditional *statement* branch (whose branch is a block regardless of the scope).
  test("named values inside sequential conditional expression branches are allowed"):
    object Test:
      @top(false) class EDProc extends EDDesign:
        val c = Bit <> IN
        val i = UInt(8) <> IN
        val o = UInt(8) <> OUT
        process(all):
          o :=
            (if (c) i + 1
             else
               val inv = ~i.bits; inv.uint)
      @top(false) class RTBody extends RTDesign:
        val c = Bit <> IN
        val i = UInt(8) <> IN
        val o = UInt(8) <> OUT
        o <>
          (if (c) i + 1
           else
             val inv = ~i.bits; inv.uint)
      @top(false) class EDStmt extends EDDesign:
        val c = Bit <> IN
        val i = UInt(8) <> IN
        val o = UInt(8) <> OUT
        process(all):
          if (c) o := i + 1
          else
            val inv = ~i.bits
            o := inv.uint
    end Test
    import Test.*
    assertElaborationErrors(EDProc())("No error found")
    assertElaborationErrors(RTBody())("No error found")
    assertElaborationErrors(EDStmt())("No error found")
  // `OPEN` carries no value, so on an input port it has nothing to drive and the net has no
  // derivable direction at all. That used to survive elaboration and blow up much later, in the
  // backend printer, as a raw stack trace.
  test("OPEN on a design instance input port"):
    class Sub extends EDDesign:
      val i = Bits(8) <> IN
      val o = Bits(8) <> OUT
      o <> i
    object Test:
      @top(false) class Top extends EDDesign:
        val o = Bits(8) <> OUT
        val sub = Sub()
        sub.i <> OPEN
        o <> sub.o
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:902:9 - 902:22
          |Hierarchy: Top
          |LHS:       sub.i
          |RHS:       OPEN
          |Message:   Cannot connect `OPEN` to sub.i.
          |`OPEN` marks an entire output port of a design instance as deliberately
          |unconnected, so it can never drive a value nor cover just part of a port.
          |To Fix:
          |* An input port of a design instance must be driven: connect a value to it.
          |* To leave only some bits of an output port unused, just do not connect them.""".stripMargin
    )
  // an output port is either read or left entirely open; a partially open one has no HDL form
  // (it used to print as an assignment to the `open` keyword itself).
  test("OPEN on part of a design instance output port"):
    class Sub extends EDDesign:
      val i = Bits(8) <> IN
      val o = Bits(8) <> OUT
      o <> i
    object Test:
      @top(false) class Top extends EDDesign:
        val i = Bits(8) <> IN
        val o = Bits(4) <> OUT
        val sub = Sub()
        sub.i <> i
        sub.o(3, 0) <> OPEN
        o <> sub.o(7, 4)
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:932:9 - 932:28
          |Hierarchy: Top
          |LHS:       sub.o(3, 0)
          |RHS:       OPEN
          |Message:   Cannot connect `OPEN` to sub.o(3, 0).
          |`OPEN` marks an entire output port of a design instance as deliberately
          |unconnected, so it can never drive a value nor cover just part of a port.
          |To Fix:
          |* An input port of a design instance must be driven: connect a value to it.
          |* To leave only some bits of an output port unused, just do not connect them.""".stripMargin
    )
  // A DFHDL `for` loop is one hardware loop, not an unrolled sequence, so a Scala `var`
  // reassigned in its body just rebinds the Scala name to a value built under the loop. Reading
  // the `var` afterwards then reaches the loop's own iterator, the loop body comes out empty, and
  // the backend prints the expression outside the loop where the iterator does not exist.
  //
  // `ScalaVarPhase` rejects the `var` form of this outright, but its view is lexical: it sees a
  // Scala name being rebound. Any other Scala-level container carries the value out of the loop
  // just as well, and this check is the backstop for those. Here a mutable collection, held in a
  // plain `val`, is filled inside the loop and read after it.
  test("value declared inside a loop read from outside it"):
    object Test:
      @top(false) class Top extends EDDesign:
        val lanes = UInt(8) X 4 <> IN
        val last = UInt(8) <> OUT
        private val picked = collection.mutable.ArrayBuffer.empty[UInt[8] <> VAL]
        process(all):
          for (i <- 0 until 4)
            picked += lanes(i)
          last := picked.last
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL scope error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:967:11 - 967:30
          |Hierarchy: Top
          |Message:   Found a read of `i`, declared inside the `for` loop at
          |${currentFilePos}ElaborationChecksSpec.scala:965:11 - 966:31, from outside that block.
          |A declaration made inside a block exists only within it. This usually comes from
          |a Scala `var` reassigned inside the block: the reassignment binds the Scala name
          |to a value built under the block, so reading the `var` afterwards reaches the
          |block's own declarations from outside.
          |To Fix:
          |Declare a DFHDL variable before the block, assign it with `:=` inside the block,
          |and read the variable afterwards.""".stripMargin
    )
  // the same accumulation written with a DFHDL variable: the loop body drives it, and the read
  // after the loop reads the variable, which is declared outside
  test("a DFHDL variable accumulated across a loop is allowed"):
    object Test:
      @top(false) class Top extends EDDesign:
        val lanes = UInt(8) X 4 <> IN
        val total = UInt(10) <> OUT
        val sum = UInt(10) <> VAR
        process(all):
          sum := 0
          for (i <- 0 until 4)
            sum := sum + lanes(i).resize(10)
          total := sum
    import Test.*
    assertElaborationErrors(Top())("No error found")

  // `DB.sharedVarCheck` Rule 1: a shared-variable write inside `process(all)` has no faithful
  // rendering (see the check's comment)
  test("shared variable write in a combinational process"):
    object Test:
      @top(false) class Top extends EDDesign:
        val a = Bits(8) <> IN
        val o = Bits(8) <> OUT
        val shr = Bits(8) <> VAR.SHARED
        process(all):
          shr :== ~a
        o <> a
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL shared variable error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1009:11 - 1009:21
          |Hierarchy: Top
          |Message:   A shared variable cannot be written inside a combinational process (`process(all)`).
          |A shared-variable write commits at the end of a clock step, so it must reside inside a clocked process.
          |""".stripMargin
    )

  // `DB.sharedVarCheck` Rule 2: a concurrent access of a shared variable never re-triggers in
  // VHDL (a shared variable is not a signal), so it is rejected under ED domains
  test("shared variable concurrent access"):
    object Test:
      @top(false) class Top extends EDDesign:
        val clk = Bit <> IN
        val d = Bits(8) <> IN
        val o = Bits(8) <> OUT
        val shr = Bits(8) <> VAR.SHARED
        process(clk):
          if (clk.rising) shr :== d
        o <> shr
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL shared variable error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1033:9 - 1033:17
          |Hierarchy: Top
          |Message:   A shared variable can only be accessed inside a process under an event-driven (ED) domain.
          |A concurrent access has no faithful VHDL rendering: a shared variable is not a signal, so its change never re-triggers a concurrent statement.
          |To Fix: move the access into a process, or use a regular variable instead.
          |""".stripMargin
    )

  // `DB.sharedVarCheck` Rule 3: no position capture can fix a guard-path hazard on an
  // RT-domain shared-variable write, so it cannot lower into the clocked process
  test("shared variable write under an unsettled guard"):
    object Test:
      @top(false) class Top extends EDDesign:
        val ram = Bits(8) X 256 <> VAR.SHARED
        val a = new RTDomain:
          val data = Bits(8) <> IN
          val addr = Bits(8) <> IN
          val we = Bit <> IN
          val gate = Bit <> VAR
          gate := we
          if (gate)
            ram(addr) := data
          gate := 0
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL shared variable error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1059:13 - 1059:30
          |Hierarchy: Top
          |Message:   A shared-variable write must lower into the clocked process, but its guard path reads a value that is reassigned later in the domain body, or it reads a `.din` value.
          |To Fix: restructure so that nothing the write's guards depend on is reassigned after the write, or hoist the guard condition computation after its operands' final assignments.
          |""".stripMargin
    )

  // `DB.sharedVarCheck` Rule 4: a loop containing a shared-variable write moves whole into the
  // clocked process, so a loop mixing combinational content must be split
  test("shared variable write in a mixed loop"):
    object Test:
      @top(false) class Top extends EDDesign:
        val ram = Bits(8) X 4 <> VAR.SHARED
        val a = new RTDomain:
          val data = Bits(8) <> IN
          val w = Bits(4) <> OUT
          COMB_LOOP:
            for (i <- 0 until 4)
              w(i) := data(i)
              ram(i) := data
    import Test.*
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL shared variable error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1084:15 - 1084:29
          |Hierarchy: Top
          |Message:   A shared-variable write inside a loop requires the whole loop to lower into the clocked process, but the loop mixes combinational content or reads values that are reassigned later in the domain body.
          |To Fix: split the loop so that the shared-variable write is in a purely-sequential loop.
          |""".stripMargin
    )

  // `sel` constructs its selection Func through a trydf-wrapped runtime helper, so a
  // candidate width mismatch must surface as a positioned elaboration error (and not as
  // an escaping derived-error exception that aborts elaboration).
  test("DFBoolOrBit sel candidate width checks"):
    object Test:
      @top(false) class SelFixed extends EDDesign:
        val c = Bit <> IN
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        // a runtime Scala Int, so the candidate width check runs at elaboration
        // (a literal would already be rejected at compile time)
        val arg = 512
        y <> c.sel(a, arg)
      end SelFixed
      @top(false) class SelParam(val W: Int <> CONST = 14) extends EDDesign:
        val c = Bit <> IN
        val b = UInt(W) <> IN
        val y = UInt(16) <> OUT
        private var acc: UInt[Int] <> VAL = d"16'0"
        for (_ <- 0 until 3) acc = acc + b
        y <> c.sel(acc, d"16'0")
      end SelParam
    end Test
    import Test.*
    assertElaborationErrors(SelFixed())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1108:9 - 1108:27
          |Hierarchy: SelFixed
          |Operation: `apply`
          |Message:   The applied RHS value width (10) is larger than the LHS variable width (8).""".stripMargin
    )
    // the accumulated width is a `max` chain the repeated-operand absorption keeps
    // minimal (`16 max W`), and the width-fit check eliminates the symbolic max operand,
    // so `16 max W >= 16` decides as `16 >= 16` and the parametric variant is accepted
    SelParam()

  test("disjoint parameter-dependent slice connections are accepted"):
    object Test:
      @top(false) class SliceParam(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        o.lsbitsAt(0, W) <> i.lsbitsAt(0, W)
        o.lsbitsAt(W, W) <> i.lsbitsAt(W, W)
      end SliceParam
      @top(false) class SliceLoop(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 3) <> IN
        val o = Bits(W * 3) <> OUT
        for (k <- 0 until 3)
          o.lsbitsAt(k * W, W) <> i.lsbitsAt(k * W, W)
      end SliceLoop
      @top(false) class SliceHiLo(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        o(W - 1, 0) <> i(W - 1, 0)
        o(2 * W - 1, W) <> i(2 * W - 1, W)
      end SliceHiLo
      @top(false) class SliceParamHigh(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W) <> IN
        val o = Bits(W) <> OUT
        o(W - 1, 1) <> i(W - 1, 1)
        o(0, 0) <> i(0, 0)
      end SliceParamHigh
      @top(false) class VecCellRanges(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W) X 4 <> IN
        val o = Bits(W) X 4 <> OUT
        o(0, 1) <> i(0, 1)
        o(2, 3) <> i(2, 3)
      end VecCellRanges
      class VecSrc(val W: Int <> CONST = 4) extends EDDesign:
        val q = Bits(W) <> OUT
        q <> all(0)
      @top(false) class VecElems(val W: Int <> CONST = 4) extends EDDesign:
        val v = Bits(W) X 3 <> VAR
        val o = Bits(W) <> OUT
        for (k <- 0 until 3)
          val s = VecSrc(W = W)
          v(k) <> s.q
        o <> v(0)
      end VecElems
      @top(false) class ProcConnMix(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        process(all):
          o.lsbitsAt(0, W) := i.lsbitsAt(0, W)
        o.lsbitsAt(W, W) <> i.lsbitsAt(W, W)
      end ProcConnMix
    end Test
    import Test.*
    SliceParam()
    SliceLoop()
    SliceHiLo()
    SliceParamHigh()
    VecCellRanges()
    VecElems()
    ProcConnMix()

  test("sub-design parameter-dependent slices resolve through applied parameters"):
    object Test:
      class MixChild(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        o(3, 0) <> i(3, 0)
        o(2 * W - 1, W) <> i(2 * W - 1, W)
      end MixChild
      @top(false) class MixParent extends EDDesign:
        val i = Bits(8) <> IN
        val o = Bits(8) <> OUT
        val c = MixChild(4)
        c.i <> i
        o <> c.o
      end MixParent
    import Test.*
    import dfhdl.compiler.stages.getCompiledCodeString
    // the backend printing itself re-derives the connectivity on the flat DB, so the
    // compiled code string (not just elaboration) is part of this regression
    assertNoDiff(
      MixParent().getCompiledCodeString,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |
         |module MixChild#(parameter int W = 4)(
         |  input  wire logic [(W * 2) - 1:0] i,
         |  output      logic [(W * 2) - 1:0] o
         |);
         |  `include "dfhdl_defs.svh"
         |  assign o[3:0] = i[3:0];
         |  assign o[(2 * W) - 1:W] = i[(2 * W) - 1:W];
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |
         |module MixParent(
         |  input  wire logic [7:0] i,
         |  output      logic [7:0] o
         |);
         |  `include "dfhdl_defs.svh"
         |  logic [(4 * 2) - 1:0] c_i;
         |  logic [(4 * 2) - 1:0] c_o;
         |  MixChild #(
         |    .W (4)
         |  ) c(
         |    .i /*<--*/ (c_i),
         |    .o /*-->*/ (c_o)
         |  );
         |  assign c_i = i;
         |  assign o   = c_o;
         |endmodule
         |""".stripMargin
    )

  test("overlapping parameter-dependent slice connections error"):
    object Test:
      @top(false) class SliceOverlap(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        o.lsbitsAt(0, W) <> i.lsbitsAt(0, W)
        o.lsbitsAt(0, W) <> i.lsbitsAt(W, W)
      end SliceOverlap
    import Test.*
    assertElaborationErrors(SliceOverlap())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1254:9 - 1254:45
          |Hierarchy: SliceOverlap
          |LHS:       o(W - 1, 0)
          |RHS:       i((W + W) - 1, W)
          |Message:   Found multiple connections write to the same variable/port `SliceOverlap.o`.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:1253:9 - 1253:45""".stripMargin
    )

  test("unprovable parameter-dependent slice connections error"):
    object Test:
      @top(false) class SliceUnprovable(val W: Int <> CONST = 4) extends EDDesign:
        val i = Bits(W * 2) <> IN
        val o = Bits(W * 2) <> OUT
        o(3, 0) <> i(3, 0)
        o(2 * W - 1, W) <> i(2 * W - 1, W)
      end SliceUnprovable
    import Test.*
    assertElaborationErrors(SliceUnprovable())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1274:9 - 1274:43
          |Hierarchy: SliceUnprovable
          |LHS:       o((2 * W) - 1, W)
          |RHS:       i((2 * W) - 1, W)
          |Message:   Found a write to the same variable/port `SliceUnprovable.o` that cannot be proven to be
          |disjoint from a previous write, because their parameter-dependent bit ranges could not be
          |resolved. If the ranges never overlap, restructure their indexing so the compiler can relate
          |them, or use assignments within a process instead of connections.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:1273:9 - 1273:27""".stripMargin
    )
  test("consistent assignment kinds per process are accepted"):
    object Test:
      @top(false) class ConsistentNB extends EDDesign:
        val clk, rst = Bit <> IN
        val d = Bit <> IN
        val q = Bit <> OUT
        process(clk.rising, rst.rising):
          if (rst) q :== 0
          else q :== d
      end ConsistentNB
      @top(false) class BlockingTemp extends EDDesign:
        val clk = Bit <> IN
        val a, b = Bits(8) <> IN
        val q = Bits(8) <> OUT
        val tmp = Bits(8) <> VAR
        process(clk.rising):
          tmp := a | b
          q :== tmp
      end BlockingTemp
      @top(false) class SplitProcesses extends EDDesign:
        val clk = Bit <> IN
        val d = Bits(8) <> IN
        val q = Bits(8) <> OUT
        process(clk.rising):
          q(3, 0) := d(3, 0)
        process(clk.rising):
          q(7, 4) :== d(7, 4)
      end SplitProcesses
    end Test
    import Test.*
    ConsistentNB()
    BlockingTemp()
    SplitProcesses()

  test("mixed assignment kinds to one variable in one process error"):
    object Test:
      @top(false) class MixedWhole extends EDDesign:
        val clk, rst = Bit <> IN
        val d = Bit <> IN
        val q = Bit <> OUT
        process(clk.rising, rst.rising):
          if (rst) q := 0
          else q :== d
      end MixedWhole
      @top(false) class MixedParts extends EDDesign:
        val clk = Bit <> IN
        val d = Bits(8) <> IN
        val q = Bits(8) <> OUT
        process(clk.rising):
          q(3, 0) := d(3, 0)
          q(7, 4) :== d(7, 4)
      end MixedParts
    end Test
    import Test.*
    assertElaborationErrors(MixedWhole())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1332:16 - 1332:23
          |Hierarchy: MixedWhole
          |LHS:       q
          |RHS:       d
          |Message:   Found both blocking (`:=`) and non-blocking (`:==`) assignments to the same variable/port `MixedWhole.q` within the same process.
          |Use one assignment kind consistently for this variable inside the process.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:1331:20 - 1331:26""".stripMargin
    )
    assertElaborationErrors(MixedParts())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1340:11 - 1340:30
          |Hierarchy: MixedParts
          |LHS:       q(7, 4)
          |RHS:       d(7, 4)
          |Message:   Found both blocking (`:=`) and non-blocking (`:==`) assignments to the same variable/port `MixedParts.q` within the same process.
          |Use one assignment kind consistently for this variable inside the process.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:1339:11 - 1339:29""".stripMargin
    )
  test("parametric max width-fit accepted via symbolic elimination"):
    object Test:
      @top(false) class MaxFitNamed(val WIDTH: Int <> CONST = 14) extends RTDesign:
        val x = UInt(WIDTH) <> IN
        val y = UInt(16) <> IN
        val sum = UInt(16) <> OUT
        val xy = x + y
        sum := xy
      end MaxFitNamed
      @top(false) class MaxFitAnon(val WIDTH: Int <> CONST = 14) extends RTDesign:
        val x = UInt(WIDTH) <> IN
        val y = UInt(16) <> IN
        val sum = UInt(16) <> OUT
        sum := x + y
      end MaxFitAnon
      @top(false) class MaxFitCarry(val WIDTH: Int <> CONST = 14) extends RTDesign:
        val x = UInt(WIDTH) <> IN
        val y = UInt(16) <> IN
        val sum20 = UInt(20) <> OUT
        sum20 := x + y
      end MaxFitCarry
      @top(false) class WidthIdentities(val W: Int <> CONST = 8) extends RTDesign:
        val a = Bits(W) <> IN
        val b = Bits(1 * W) <> OUT
        val c = Bits(W + 0) <> OUT
        val d = Bits(W - 0) <> OUT
        val z = Bits(0 * W + 4) <> OUT
        b := a
        c := a
        d := a
        z := h"4'0"
      end WidthIdentities
    end Test
    import Test.*
    MaxFitNamed()
    MaxFitAnon()
    MaxFitCarry()
    WidthIdentities()

  test("parametric max width-fit rejections"):
    object Test:
      @top(false) class MaxTooNarrow(val WIDTH: Int <> CONST = 14) extends RTDesign:
        val x = UInt(WIDTH) <> IN
        val y = UInt(16) <> IN
        val sum15 = UInt(15) <> OUT
        val xy = x + y
        sum15 := xy
      end MaxTooNarrow
      @top(false) class PlainSymWidth(val WIDTH: Int <> CONST = 14) extends RTDesign:
        val x = UInt(WIDTH) <> IN
        val sum = UInt(16) <> OUT
        sum := x
      end PlainSymWidth
    import Test.*
    assertElaborationErrors(MaxTooNarrow())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1412:9 - 1412:20
          |Hierarchy: MaxTooNarrow
          |Operation: `:=`
          |Message:   The applied RHS value width (WIDTH max 16) is larger than the LHS variable width (15).""".stripMargin
    )
    assertElaborationErrors(PlainSymWidth())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1417:9 - 1417:17
          |Hierarchy: PlainSymWidth
          |Operation: `:=`
          |Message:   The applied RHS value width (WIDTH) is undefined compared to the LHS variable width (16).""".stripMargin
    )

  test("same-named width constants are qualified in DFBits width errors"):
    object Test:
      @top(false) class Child(val W: Int <> CONST = 4) extends EDDesign:
        val OUTPUT_WIDTH = W * 2
        val o = Bits(OUTPUT_WIDTH) <> OUT
        o <> all(0)
      end Child
      @top(false) class Parent(val W: Int <> CONST = 8) extends EDDesign:
        val OUTPUT_WIDTH = W
        val o = Bits(OUTPUT_WIDTH) <> OUT
        val c = Child(W = 4)
        o <> c.o
      end Parent
    import Test.*
    assertElaborationErrors(Parent())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1448:9 - 1448:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The argument width (c.OUTPUT_WIDTH) is different than the receiver width (OUTPUT_WIDTH).
          |Consider applying `.resize` to resolve this issue.
          |
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1448:9 - 1448:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The argument width (OUTPUT_WIDTH) is different than the receiver width (c.OUTPUT_WIDTH).
          |Consider applying `.resize` to resolve this issue.""".stripMargin
    )

  test("same-named width constants are qualified in DFDecimal width errors"):
    object Test:
      @top(false) class Child(val W: Int <> CONST = 4) extends EDDesign:
        val OUTPUT_WIDTH = W * 2
        val o = UInt(OUTPUT_WIDTH) <> OUT
        o <> 0
      end Child
      @top(false) class Parent(val W: Int <> CONST = 8) extends EDDesign:
        val OUTPUT_WIDTH = W
        val o = UInt(OUTPUT_WIDTH) <> OUT
        val c = Child(W = 4)
        o <> c.o
      end Parent
    import Test.*
    assertElaborationErrors(Parent())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1479:9 - 1479:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The applied RHS value width (c.OUTPUT_WIDTH) is undefined compared to the LHS variable width (OUTPUT_WIDTH).
          |
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1479:9 - 1479:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The applied RHS value width (OUTPUT_WIDTH) is undefined compared to the LHS variable width (c.OUTPUT_WIDTH).""".stripMargin
    )

  test("same-named design parameters are qualified in width errors"):
    object Test:
      @top(false) class Child(val W: Int <> CONST = 8) extends EDDesign:
        val o = Bits(W) <> OUT
        o <> all(0)
      end Child
      @top(false) class Parent(val W: Int <> CONST = 8) extends EDDesign:
        val o = Bits(W) <> OUT
        val c = Child(W = 4)
        o <> c.o
      end Parent
    import Test.*
    assertElaborationErrors(Parent())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1506:9 - 1506:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The argument width (c.W) is different than the receiver width (W).
          |Consider applying `.resize` to resolve this issue.
          |
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1506:9 - 1506:17
          |Hierarchy: Parent
          |Operation: `apply`
          |Message:   The argument width (W) is different than the receiver width (c.W).
          |Consider applying `.resize` to resolve this issue.""".stripMargin
    )

  test("Verilog-semantics warning with parametric widths"):
    object Test:
      @top(false) class ParW(val CORDW: Int <> CONST = 16) extends EDDesign:
        val err = SInt(CORDW + 1) <> IN
        val dy = SInt(CORDW + 1) <> IN
        val t = 2 * err >= dy
      end ParW
      @top(false) class ParWDiv(val CORDW: Int <> CONST = 16) extends EDDesign:
        val a = UInt(CORDW) <> IN
        val b = UInt(CORDW) <> IN
        val t = (a + b) / 4
      end ParWDiv
    import Test.*
    val warnMsg =
      """|Implicit Scala/DFHDL Int conversion may produce different results than Verilog.
         |In Verilog, integer literals are 32-bit, which can widen intermediate arithmetic.
         |In DFHDL, Int literals are converted to minimum bit-accurate width.
         |Use carry operations (+^, -^, *^) or explicit bit-accurate literals (d"W'V").""".stripMargin
    def assertWarns(dsn: dfhdl.core.Design, expected: String*): Unit =
      val warns = dsn.dfc.getWarnings.map(_.dfMsg)
      assertEquals(warns.length, expected.length)
      warns.lazyZip(expected).foreach(assertNoDiff(_, _))
    // the parametric width resolves through the design parameter's applied (or default)
    // value at elaboration, so the warning fires exactly as with a literal width
    assertWarns(ParW(), warnMsg)
    assertWarns(ParWDiv(), warnMsg)
    // a parametric width that resolves to 32 bits or wider stays suppressed
    assertWarns(ParW(31))

  test("parametric width-fit proof rejections"):
    object Test:
      @top(false) class MulTooNarrow(val W: Int <> CONST = 8) extends EDDesign:
        val a, b = SInt(W) <> IN
        val prod16 = SInt(16) <> OUT
        prod16 <> a * b
      end MulTooNarrow
      @top(false) class ProvablyNarrow(val W: Int <> CONST = 8) extends RTDesign:
        val x = SInt(2 * W) <> IN
        val narrow = SInt(W) <> OUT
        narrow := x
      end ProvablyNarrow
    import Test.*
    // a literal target against a free parameter stays undecidable: a valid W may exceed it
    assertElaborationErrors(MulTooNarrow())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1560:9 - 1560:24
          |Hierarchy: MulTooNarrow
          |Operation: `apply`
          |Message:   The applied RHS value width (W) is undefined compared to the LHS variable width (16).""".stripMargin
    )
    // the width-fit proof decides the negative direction definitively: W >= 2 * W is
    // violated for every valid W, so the vague "undefined" error upgrades to "larger than"
    assertElaborationErrors(ProvablyNarrow())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:1565:9 - 1565:20
          |Hierarchy: ProvablyNarrow
          |Operation: `:=`
          |Message:   The applied RHS value width (2 * W) is larger than the LHS variable width (W).""".stripMargin
    )

end ElaborationChecksSpec
