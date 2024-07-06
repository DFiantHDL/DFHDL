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
end ElaborationChecksSpec
