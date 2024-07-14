import dfhdl.*
import munit.*

class ElaborationChecksSpec extends DesignSpec:
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
      """|Elaboration errors found!
         |DFiant HDL elaboration error!
         |Position:  ElaborationChecksSpec.scala:58:23 - 58:31
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
      """|Elaboration errors found!
         |DFiant HDL name errors!
         |Unable to determine names for the members declared at the following positions:
         |ElaborationChecksSpec.scala:72:11 - 72:19
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
      """|Elaboration errors found!
         |DFiant HDL connectivity error!
         |Position:  ElaborationChecksSpec.scala:113:9 - 113:15
         |Hierarchy: Top
         |LHS:       x
         |RHS:       0
         |Message:   Found multiple domain assignments to the same variable/port `Top.x`
         |Only variables declared as `VAR.SHARED` under ED domain allow this.
         |The previous write occurred at ElaborationChecksSpec.scala:109:9 - 109:15
         |
         |DFiant HDL connectivity error!
         |Position:  ElaborationChecksSpec.scala:114:9 - 114:15
         |Hierarchy: Top
         |LHS:       y
         |RHS:       0
         |Message:   Found multiple domain assignments to the same variable/port `Top.y`
         |Only variables declared as `VAR.SHARED` under ED domain allow this.
         |The previous write occurred at ElaborationChecksSpec.scala:110:9 - 110:15
         |""".stripMargin
    )
end ElaborationChecksSpec
