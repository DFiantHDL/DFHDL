package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropOpaques
import dfhdl.compiler.stages.dropMagnets
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

//TODO: fix tests with magnet types
class DropOpaquesSpec extends StageSpec:
  // Test opaque types
  case class MyOpaque() extends Opaque(UInt(32))
  case class Counter() extends Opaque(UInt(32))
  case class Address() extends Opaque(UInt(32))

  // Test magnet types
  case class M1() extends Magnet(Bit)
  case class M2() extends Magnet(UInt(32))

  test("Basic design with opaque types - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> IN
      val y = Counter  <> OUT
      val z = Address  <> VAR
      process(all):
        y.actual :== x.actual
        z.actual :== x.actual
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    y :== x
         |    z :== x
         |end Top""".stripMargin
    )
  }

  // test("Basic design with magnet types - drop magnets only") {
  //   class Top extends EDDesign:
  //     val x = M1       <> IN
  //     val y = M2       <> OUT
  //     val z = MyOpaque <> VAR
  //     process(all):
  //       y.actual :== x.actual
  //       z.actual :== x.actual
  //   val top = (new Top).dropMagnets
  //   assertCodeString(
  //     top,
  //     """|class Top extends EDDesign:
  //        |  val x = Bit <> IN
  //        |  val y = UInt(4) <> OUT
  //        |  val z = MyOpaque <> VAR
  //        |  process(all):
  //        |    y :== x
  //        |    z.actual :== x
  //        |end Top
  //        |""".stripMargin
  //   )
  // }

  test("Opaque casting operations - drop all opaques") {
    class Top extends EDDesign:
      val x = UInt(32) <> VAR
      val y = Counter  <> VAR
      val z = MyOpaque <> VAR
      process(all):
        // Test x.as(MyOpaque) -> x
        z.actual :== x.as(MyOpaque).actual
        // Test y.actual -> y (when y is opaque)
        y.actual :== x
        // Test direct assignment
        z.actual :== x
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    z :== x
         |    y :== x
         |    z :== x
         |end Top
         |""".stripMargin
    )
  }

  test("Complex hierarchy with opaque types - drop all opaques") {
    class Inner extends EDDesign:
      val x = MyOpaque <> IN
      val y = Counter  <> OUT
      val z = Address  <> VAR
      process(all):
        y.actual :== x.actual
        z.actual :== x.actual
    class Middle extends EDDesign:
      val x     = MyOpaque <> IN
      val y     = Counter  <> OUT
      val inner = Inner()
      inner.x <> x
      y       <> inner.y
    class Top extends EDDesign:
      val x      = MyOpaque <> IN
      val y      = Counter  <> OUT
      val middle = Middle()
      middle.x <> x
      y        <> middle.y
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Inner extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    y :== x
         |    z :== x
         |end Inner
         |
         |class Middle extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val inner = Inner()
         |  inner.x <> x
         |  y <> inner.y
         |end Middle
         |
         |class Top extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val middle = Middle()
         |  middle.x <> x
         |  y <> middle.y
         |end Top
         |""".stripMargin
    )
  }

  // test("Mixed opaque and magnet types - drop magnets only") {
  //   class Top extends EDDesign:
  //     val x = M1       <> IN
  //     val y = M2       <> OUT
  //     val z = MyOpaque <> VAR
  //     val w = Counter  <> VAR
  //     process(all):
  //       y.actual :== x.actual
  //       z.actual :== x.actual
  //       w.actual :== x.actual
  //   val top = (new Top).dropMagnets
  //   assertCodeString(
  //     top,
  //     """|class Top extends EDDesign:
  //        |  val x = Bit <> IN
  //        |  val y = UInt(4) <> OUT
  //        |  val z = MyOpaque <> VAR
  //        |  val w = Counter <> VAR
  //        |  process(all):
  //        |    y :== x
  //        |    z.actual :== x
  //        |    w.actual :== x
  //        |end Top
  //        |""".stripMargin
  //   )
  // }

  test("Opaque types with complex operations - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> VAR
      val y = Counter  <> VAR
      val z = Address  <> VAR
      process(all):
        // Test complex expressions with opaque types
        val temp1 = x.actual + 1
        val temp2 = y.actual * 2
        val temp3 = z.actual - 10
        x.actual :== temp1
        y.actual :== temp2
        z.actual :== temp3
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    val temp1 = x + d"32'1"
         |    val temp2 = y * d"32'2"
         |    val temp3 = z - d"32'10"
         |    x :== temp1
         |    y :== temp2
         |    z :== temp3
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types in vectors - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque X 4 <> VAR
      val y = Counter X 2  <> VAR
      process(all):
        x(0).actual :== 1
        x(1).actual :== 2
        y(0).actual :== 100
        y(1).actual :== 200
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) X 4 <> VAR
         |  val y = UInt(32) X 2 <> VAR
         |  process(all):
         |    x(0) :== d"32'1"
         |    x(1) :== d"32'2"
         |    y(0) :== d"32'100"
         |    y(1) :== d"32'200"
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types in structs - drop all opaques") {
    case class MyStruct(x: MyOpaque <> VAL, y: Counter <> VAL, z: Address <> VAL) extends Struct
    class Top extends EDDesign:
      val s = MyStruct <> VAR
      process(all):
        s.x.actual :== 1
        s.y.actual :== 100
        s.z.actual :== 1000
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  final case class MyStruct(
         |      x: UInt[32] <> VAL
         |      y: UInt[32] <> VAL
         |      z: UInt[32] <> VAL
         |  ) extends Struct
         |
         |  val s = MyStruct <> VAR
         |  process(all):
         |    s.x :== d"32'1"
         |    s.y :== d"32'100"
         |    s.z :== d"32'1000"
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with conditional logic - drop all opaques") {
    class Top extends EDDesign:
      val x    = MyOpaque <> VAR
      val y    = Counter  <> VAR
      val z    = Address  <> VAR
      val cond = Bit      <> VAR
      process(all):
        if (cond)
          x.actual :== 1
          y.actual :== 100
        else
          x.actual :== 2
          y.actual :== 200
        z.actual :== x.actual
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  val cond = Bit <> VAR
         |  process(all):
         |    if (cond)
         |      x :== d"32'1"
         |      y :== d"32'100"
         |    else
         |      x :== d"32'2"
         |      y :== d"32'200"
         |    end if
         |    z :== x
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with function calls - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> VAR
      val y = Counter  <> VAR
      val z = Address  <> VAR
      process(all):
        val temp1 = x.actual + y.actual
        val temp2 = z.actual - x.actual
        x.actual :== temp1
        z.actual :== temp2
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    val temp1 = x + y
         |    val temp2 = z - x
         |    x :== temp1
         |    z :== temp2
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with ports and connections - drop all opaques") {
    class Inner extends EDDesign:
      val x = MyOpaque <> IN
      val y = Counter  <> OUT
      process(all):
        y.actual :== x.actual
    class Top extends EDDesign:
      val x     = MyOpaque <> IN
      val y     = Counter  <> OUT
      val inner = Inner()
      inner.x <> x
      y       <> inner.y
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Inner extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  process(all):
         |    y :== x
         |end Inner
         |
         |class Top extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val inner = Inner()
         |  inner.x <> x
         |  y <> inner.y
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with initial values - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> VAR init 1.as(MyOpaque)
      val y = Counter  <> VAR init 100.as(Counter)
      val z = Address  <> VAR init 1000.as(Address)
      process(all):
        x.actual :== x.actual + 1
        y.actual :== y.actual + 1
        z.actual :== z.actual + 1
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR init d"32'1"
         |  val y = UInt(32) <> VAR init d"32'100"
         |  val z = UInt(32) <> VAR init d"32'1000"
         |  process(all):
         |    x :== x + d"32'1"
         |    y :== y + d"32'1"
         |    z :== z + d"32'1"
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with bit operations - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> VAR
      val y = Counter  <> VAR
      process(all):
        val temp1 = x.actual & h"000000ff"
        val temp2 = y.actual >> 2
        x.actual :== temp1
        y.actual :== temp2
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  process(all):
         |    val temp1 = x.bits & h"000000ff"
         |    val temp2 = y >> 2
         |    x :== temp1.uint
         |    y :== temp2
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with comparison operations - drop all opaques") {
    class Top extends EDDesign:
      val x      = MyOpaque <> VAR
      val y      = Counter  <> VAR
      val z      = Address  <> VAR
      val result = Bit      <> VAR
      process(all):
        result :== x.actual > y.actual
        z.actual :== (if (x.actual == y.actual) 0 else 1)
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  val result = Bit <> VAR
         |  process(all):
         |    result :== (x > y).bit
         |    z :== ((
         |      if (x == y) d"32'0"
         |      else d"32'1"
         |    ): UInt[32] <> VAL)
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with nested structures - drop all opaques") {
    case class InnerStruct(x: MyOpaque <> VAL, y: Counter <> VAL) extends Struct
    case class OuterStruct(inner: InnerStruct <> VAL, z: Address <> VAL) extends Struct
    class Top extends EDDesign:
      val s = OuterStruct <> VAR
      process(all):
        s.inner.x.actual :== 1
        s.inner.y.actual :== 100
        s.z.actual :== 1000
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  final case class InnerStruct(
         |      x: UInt[32] <> VAL
         |      y: UInt[32] <> VAL
         |  ) extends Struct
         |  final case class OuterStruct(
         |      inner: InnerStruct <> VAL
         |      z: UInt[32] <> VAL
         |  ) extends Struct
         |
         |  val s = OuterStruct <> VAR
         |  process(all):
         |    s.inner.x :== d"32'1"
         |    s.inner.y :== d"32'100"
         |    s.z :== d"32'1000"
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with vectors and indexing - drop all opaques") {
    class Top extends EDDesign:
      val x     = MyOpaque X 4 <> VAR
      val y     = Counter X 2  <> VAR
      val z     = Address      <> VAR
      val index = UInt(2)      <> VAR
      process(all):
        z.actual :== x(index).actual
        y(0).actual :== x(0).actual + x(1).actual
        y(1).actual :== x(2).actual + x(3).actual
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) X 4 <> VAR
         |  val y = UInt(32) X 2 <> VAR
         |  val z = UInt(32) <> VAR
         |  val index = UInt(2) <> VAR
         |  process(all):
         |    z :== x(index.toInt)
         |    y(0) :== x(0) + x(1)
         |    y(1) :== x(2) + x(3)
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with multiple assignments - drop all opaques") {
    class Top extends EDDesign:
      val x = MyOpaque <> VAR
      val y = Counter  <> VAR
      val z = Address  <> VAR
      process(all):
        x.actual :== 1
        y.actual :== 100
        z.actual :== 1000
        // Multiple assignments to same variable
        x.actual :== x.actual + 1
        y.actual :== y.actual * 2
        z.actual :== z.actual - 10
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  process(all):
         |    x :== d"32'1"
         |    y :== d"32'100"
         |    z :== d"32'1000"
         |    x :== x + d"32'1"
         |    y :== y * d"32'2"
         |    z :== z - d"32'10"
         |end Top
         |""".stripMargin
    )
  }

  test("Opaque types with complex expressions - drop all opaques") {
    class Top extends EDDesign:
      val x      = MyOpaque <> VAR
      val y      = Counter  <> VAR
      val z      = Address  <> VAR
      val result = UInt(32) <> VAR
      process(all):
        // Complex expression with multiple opaque types
        result :== (x.actual + y.actual) * z.actual
        x.actual :== result & h"000000ff"
        y.actual :== (result >> 8) & h"0000ffff"
        z.actual :== (result >> 24) & h"000000ff"
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = UInt(32) <> VAR
         |  val y = UInt(32) <> VAR
         |  val z = UInt(32) <> VAR
         |  val result = UInt(32) <> VAR
         |  process(all):
         |    result :== (x + y) * z
         |    x :== (result.bits & h"000000ff").uint
         |    y :== ((result >> 8).bits & h"0000ffff").uint
         |    z :== ((result >> 24).bits & h"000000ff").uint
         |end Top
         |""".stripMargin
    )
  }

  test("Constant global vector of opaque types - drop all opaques") {
    val g: MyOpaque X 4 <> CONST = Vector(1, 2, 3, 4).map(i => i.as(MyOpaque))
    class Top extends RTDesign:
      val o = MyOpaque <> OUT
      o := g(0)
    val top = (new Top).dropOpaques
    assertCodeString(
      top,
      """|val g: UInt[32] X 4 <> CONST = DFVector(UInt(32) X 4)(d"32'1", d"32'2", d"32'3", d"32'4")
         |class Top extends RTDesign:
         |  val o = UInt(32) <> OUT
         |  o := g(0)
         |end Top
         |""".stripMargin
    )
  }

end DropOpaquesSpec
