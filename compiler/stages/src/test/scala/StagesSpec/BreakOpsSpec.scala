package StagesSpec

import dfhdl.*
import compiler.stages.breakOpsWithAssignments
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class BreakOpsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Break vector concatenations") {
    case class Foo() extends Opaque(Bits(8) X 4 X 4)
    class ID extends DFDesign:
      val x = Bits(8)         <> IN
      val y = Bits(8)         <> OUT
      val z = Bits(8) X 4 X 4 <> VAR

      @inline def anon(arg: Bits[8] X 4 X 4 <> VAL): Bits[8] X 4 X 4 <> DFRET = arg
      z := anon(Vector.fill(4)(Vector(h"11", h"22", x, h"44")))
      y := anon(Vector.fill(4)(Vector(h"11", h"22", x, h"44"))).as(Foo).actual(1)(3)
    val id = (new ID).breakOpsWithAssignments
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  case class Foo() extends Opaque(Bits(8) X 4 X 4)
         |
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val z = Bits(8) X 4 X 4 <> VAR
         |  z(0)(0) := h"11"
         |  z(0)(1) := h"22"
         |  z(0)(2) := x
         |  z(0)(3) := h"44"
         |  z(1)(0) := h"11"
         |  z(1)(1) := h"22"
         |  z(1)(2) := x
         |  z(1)(3) := h"44"
         |  z(2)(0) := h"11"
         |  z(2)(1) := h"22"
         |  z(2)(2) := x
         |  z(2)(3) := h"44"
         |  z(3)(0) := h"11"
         |  z(3)(1) := h"22"
         |  z(3)(2) := x
         |  z(3)(3) := h"44"
         |  y := h"44"
         |end ID
         |""".stripMargin
    )
  }
  test("Break struct concatenations") {
    case class Foo(arg1: Bits[8] <> VAL, arg2: Bit <> VAL) extends Struct
    case class Bar(foo1: Foo <> VAL, foo2: Foo <> VAL) extends Struct
    class ID extends DFDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      val z = Bar     <> VAR

      @inline def anon(arg: Bar <> VAL): Bar <> DFRET = arg
      z := anon(Bar(Foo(h"11", 1), Foo(h"22", 0)))
      y := anon(Bar(Foo(h"11", 1), Foo(x, 0))).foo2.arg1
    val id = (new ID).breakOpsWithAssignments
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  final case class Foo(
         |      arg1: Bits[8] <> VAL
         |      arg2: Bit <> VAL
         |  ) extends Struct
         |  final case class Bar(
         |      foo1: Foo <> VAL
         |      foo2: Foo <> VAL
         |  ) extends Struct
         |
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val z = Bar <> VAR
         |  z.foo1.arg1 := h"11"
         |  z.foo1.arg2 := 1
         |  z.foo2.arg1 := h"22"
         |  z.foo2.arg2 := 0
         |  y := x
         |end ID
         |""".stripMargin
    )
  }
end BreakOpsSpec
