import dfhdl.*
import munit.*
import java.io.File.separatorChar as S
given options.ElaborationOptions.OnError = options.OnError.Exception
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
         |Top.internal2.dmn
         |Sources:
         |Top.internal1.dmn1
         |Top.internal1.dmn2
         |Possible solution:
         |Either explicitly define a configuration for the domain or drive it from a single source domain.
         |""".stripMargin
    )

  test("cyclic RT dependency errors"):
    class Internal extends EDDesign:
      val dmn = new RTDomain:
        val i = Bit <> IN
        val o = Bit <> OUT
        o := i
    object Test:
      @top(false) class Top extends EDDesign:
        val internal1 = Internal()
        val internal2 = Internal()
        internal1.dmn.i <> internal2.dmn.o
        internal1.dmn.o <> internal2.dmn.i
    import Test.*
    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |Circular derived RT configuration detected. Involved in the cycle:
         |Top.internal1.dmn
         |Top.internal2.dmn
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
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:64:25 - 64:33
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
          |${currentFilePos}ElaborationChecksSpec.scala:80:13 - 80:21
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
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:123:11 - 123:17
          |Hierarchy: Top
          |LHS:       x
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.x`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:119:11 - 119:17
          |
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:124:11 - 124:17
          |Hierarchy: Top
          |LHS:       y
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.y`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:120:11 - 120:17
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
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:154:19 - 154:28
          |Hierarchy: Top.y
          |Operation: ``
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
    assertElaborationErrors(IDTop())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:176:18 - 176:20
          |Hierarchy: IDTop.id
          |Message:   Found a dangling (unconnected) input port `x`.
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:176:18 - 176:20
          |Hierarchy: IDTop.id
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
          |${currentFilePos}ElaborationChecksSpec.scala:195:9 - 195:18
          |${currentFilePos}ElaborationChecksSpec.scala:197:9 - 197:26
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
          |Position:  lib${S}src${S}test${S}scala${S}ElaborationChecksSpec.scala:237:11 - 237:21
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
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:259:17 - 259:27
          |Hierarchy: Top
          |Message:   Found a latch variable `y`. Latches are not allowed under RT domains.""".stripMargin
    )
end ElaborationChecksSpec
