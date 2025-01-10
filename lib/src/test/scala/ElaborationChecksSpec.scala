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
    @top(false) class Top extends EDDesign:
      val internal1 = Internal1()
      val internal2 = Internal2()
      internal1.dmn1.o <> internal2.dmn.i1
      internal1.dmn2.o <> internal2.dmn.i2

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
    @top(false) class Top extends EDDesign:
      val internal1 = Internal()
      val internal2 = Internal()
      internal1.dmn.i <> internal2.dmn.o
      internal1.dmn.o <> internal2.dmn.i

    assertElaborationErrors(Top())(
      """|Elaboration errors found!
         |Circular derived RT configuration detected. Involved in the cycle:
         |Top.internal1.dmn
         |Top.internal2.dmn
         |""".stripMargin
    )

  test("domain creation in the wrong spot"):
    @top(false) class Top extends RTDesign:
      val x = Boolean <> IN
      if (x)
        val dmn = new RTDomain {}
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:60:23 - 60:31
          |Hierarchy: Top.dmn
          |Operation: `apply`
          |Message:   A domain can only be directly owned by a design, an interface, or another domain.
          |""".stripMargin
    )

  test("anonymous domains are forbidden"):
    @top(false) class Top extends RTDesign:
      new RTDomain {} setName ("someName")
      new RTDomain {}
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL name errors!
          |Unable to determine names for the members declared at the following positions:
          |${currentFilePos}ElaborationChecksSpec.scala:74:11 - 74:19
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
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:115:9 - 115:15
          |Hierarchy: Top
          |LHS:       x
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.x`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:111:9 - 111:15
          |
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:116:9 - 116:15
          |Hierarchy: Top
          |LHS:       y
          |RHS:       0
          |Message:   Found multiple domain assignments to the same variable/port `Top.y`.
          |Only variables declared as `VAR.SHARED` under ED domain allow this.
          |The previous write occurred at ${currentFilePos}ElaborationChecksSpec.scala:112:9 - 112:15
          |""".stripMargin
    )

  test("port declaration in the wrong spot"):
    @top(false) class Top extends RTDesign:
      val x = Boolean <> IN
      if (x)
        val y = Bit <> IN
    assertElaborationErrors(Top())(
      s"""|Elaboration errors found!
          |DFiant HDL elaboration error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:144:17 - 144:26
          |Hierarchy: Top.y
          |Operation: ``
          |Message:   Ports can only be directly owned by a design, a domain or an interface.
          |""".stripMargin
    )

  test("dangling input"):
    class ID extends EDDesign:
      val x = Bits(10) <> IN
      val y = Bits(10) <> OUT
      y <> x

    @top class IDTop extends EDDesign:
      val x = Bits(10) <> IN
      val y = Bits(10) <> OUT

      val id = ID()
      id.y <> y
    assertElaborationErrors(IDTop())(
      s"""|Elaboration errors found!
          |DFiant HDL connectivity error!
          |Position:  ${currentFilePos}ElaborationChecksSpec.scala:165:16 - 165:18
          |Hierarchy: IDTop.id
          |Message:   Found a dangling (unconnected) input port `x`.
          |""".stripMargin
    )
end ElaborationChecksSpec
