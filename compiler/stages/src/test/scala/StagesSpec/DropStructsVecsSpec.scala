package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropStructsVecs
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropStructsVecsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Drop vector") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    class Foo extends DFDesign:
      val x  = UInt(8) X 4     <> IN
      val y  = UInt(8) X 4     <> OUT
      val y2 = UInt(8) X 3     <> OUT
      val xx = UInt(8) X 4 X 4 <> IN
      val z0 = UInt(8)         <> OUT
      val z3 = UInt(8)         <> OUT
      z0 := x(0)
      val xx0  = xx(0)
      val xx00 = xx0(0)
      z3 := xx(3)(1)
      y  := x
      y2 := x(1, 3)
    val top = (new Foo).dropStructsVecs
    assertCodeString(
      top,
      """|class Foo extends DFDesign:
         |  val x = Bits(32) <> IN
         |  val y = Bits(32) <> OUT
         |  val y2 = Bits(24) <> OUT
         |  val xx = Bits(128) <> IN
         |  val z0 = UInt(8) <> OUT
         |  val z3 = UInt(8) <> OUT
         |  z0 := x(31, 24).uint
         |  val xx0 = xx(127, 96)
         |  val xx00 = xx0(31, 24).uint
         |  z3 := xx(23, 16).uint
         |  y := x
         |  y2 := x(23, 0)
         |end Foo
         |""".stripMargin
    )
  }

  test("Drop vector with parameters") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    class VectorWithParams(
        val width: Int <> CONST = 8,
        val depth: Int <> CONST = 4
    ) extends DFDesign:
      val x  = UInt(width) X depth         <> IN
      val y  = UInt(width) X depth         <> OUT
      val xx = UInt(width) X depth X depth <> IN
      val z0 = UInt(width)                 <> OUT
      val z3 = UInt(width)                 <> OUT
      z0 := x(0)
      z3 := xx(3)(1)
      y  := x
    val top = (new VectorWithParams).dropStructsVecs
    assertCodeString(
      top,
      """|class VectorWithParams(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 4
         |) extends DFDesign:
         |  val x = Bits(width * depth) <> IN
         |  val y = Bits(width * depth) <> OUT
         |  val xx = Bits((width * depth) * depth) <> IN
         |  val z0 = UInt(width) <> OUT
         |  val z3 = UInt(width) <> OUT
         |  z0 := x((width + (((width * depth) - (width * 1)) + 0)) - 1, ((width * depth) - (width * 1)) + 0).uint
         |  z3 := xx((width + ((((width * depth) * depth) - ((width * depth) * 4)) + (((width * depth) - (width * 2)) + 0))) - 1, (((width * depth) * depth) - ((width * depth) * 4)) + (((width * depth) - (width * 2)) + 0)).uint
         |  y := x
         |end VectorWithParams
         |""".stripMargin
    )
  }

  test("Ignore block ram access vectors") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    class BlockRam(val width: Int <> CONST = 8, val depth: Int <> CONST = 4) extends DFDesign:
      val v =
        UInt(width) X depth <> VAR init h"${width}'0".repeat(depth).as(UInt(width) X depth)
      val v2  = UInt(width) X depth <> VAR init all(0)
      val v3  = UInt(width) X depth <> VAR init Vector(0, 1, 2, 3)
      val sel = UInt.until(depth)   <> IN
      val o   = UInt(width)         <> OUT
      val o2  = UInt(width) X depth <> OUT
      o  := v(sel) + v2(sel)
      o2 := v3
    val top = (new BlockRam).dropStructsVecs
    assertCodeString(
      top,
      """|class BlockRam(
         |    val width: Int <> CONST = 8,
         |    val depth: Int <> CONST = 4
         |) extends DFDesign:
         |  val v = UInt(width) X depth <> VAR init h"${width}'0".repeat(depth).as(UInt(width) X depth)
         |  val v2 = UInt(width) X depth <> VAR init h"${width}'0".repeat(depth).as(UInt(width) X depth)
         |  val v3 = Bits(width * depth) <> VAR init (h"${width}'0", h"${width}'1", h"${width}'2", h"${width}'3").toBits
         |  val sel = UInt(clog2(depth)) <> IN
         |  val o = UInt(width) <> OUT
         |  val o2 = Bits(width * depth) <> OUT
         |  o := v(sel.toInt) + v2(sel.toInt)
         |  o2 := v3
         |end BlockRam
         |""".stripMargin
    )
  }

  test("Drop struct") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    case class Point(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
    class StructExample extends DFDesign:
      val p = Point   <> IN
      val q = Point   <> OUT
      val y = UInt(8) <> OUT
      val x = UInt(8) <> OUT
      y := p.y
      x := p.x
      q := p
    val top = (new StructExample).dropStructsVecs
    assertCodeString(
      top,
      """|class StructExample extends DFDesign:
         |  val p = Bits(16) <> IN
         |  val q = Bits(16) <> OUT
         |  val y = UInt(8) <> OUT
         |  val x = UInt(8) <> OUT
         |  y := p(7, 0).uint
         |  x := p(15, 8).uint
         |  q := p
         |end StructExample
         |""".stripMargin
    )
  }

  test("Drop struct with parameters") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    val w: Int <> CONST                   = 8
    case class Point(x: UInt[w.type] <> VAL, y: UInt[w.type] <> VAL) extends Struct
    class StructExample extends DFDesign:
      val p = Point   <> IN
      val q = Point   <> OUT
      val y = UInt(w) <> OUT
      val x = UInt(w) <> OUT
      y := p.y
      x := p.x
      q := p
    val top = (new StructExample).dropStructsVecs
    assertCodeString(
      top,
      """|val w: Int <> CONST = 8
         |
         |class StructExample extends DFDesign:
         |  val p = Bits(w + w) <> IN
         |  val q = Bits(w + w) <> OUT
         |  val y = UInt(w) <> OUT
         |  val x = UInt(w) <> OUT
         |  y := p((w + 0) - 1, 0).uint
         |  x := p((w + (0 + w)) - 1, 0 + w).uint
         |  q := p
         |end StructExample
         |""".stripMargin
    )
  }

  test("Drop complex composition") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    case class Point(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
    case class VectorHolder(vec: UInt[8] X 4 <> VAL) extends Struct
    case class ComplexStruct(
        points: Point X 3 <> VAL,
        holder: VectorHolder <> VAL
    ) extends Struct
    class ComplexExample extends DFDesign:
      val input  = ComplexStruct <> IN
      val output = ComplexStruct <> OUT
      val x      = UInt(8)       <> OUT
      val y      = UInt(8)       <> OUT
      val z      = UInt(8) X 2   <> OUT
      x      := input.points(0).x
      y      := input.points(2).y
      z      := input.holder.vec(1, 2)
      output := input
    val top = (new ComplexExample).dropStructsVecs
    assertCodeString(
      top,
      """|class ComplexExample extends DFDesign:
         |  val input = Bits(80) <> IN
         |  val output = Bits(80) <> OUT
         |  val x = UInt(8) <> OUT
         |  val y = UInt(8) <> OUT
         |  val z = Bits(16) <> OUT
         |  x := input(79, 72).uint
         |  y := input(39, 32).uint
         |  z := input(23, 8)
         |  output := input
         |end ComplexExample
         |""".stripMargin
    )
  }

  test("Global constant vector") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    val arg: Bits[8] X 4 <> CONST         = Vector(h"01", h"02", h"03", h"04")
    val arg2: Bits[8] X 4 X 4 <> CONST    = Vector(
      Vector(h"01", h"02", h"03", h"04"),
      Vector(h"05", h"06", h"07", h"08"),
      Vector(h"09", h"10", h"11", h"12"),
      Vector(h"13", h"14", h"15", h"16")
    )
    class Bar() extends RTDesign:
      val o   = Bits(8)         <> OUT
      val o2  = Bits(8) X 4     <> OUT
      val o3  = Bits(8) X 4     <> OUT
      val v   = Bits(8) X 4     <> VAR init arg
      val v2  = Bits(8) X 4 X 4 <> VAR init arg2
      val v3  = Bits(8) X 4     <> VAR.REG
      val sel = UInt.until(4)   <> IN
      o           := arg(0) ^ arg(1) ^ v(sel) ^ v3(sel)
      o2          := arg2(0)
      v3(sel).din := v(sel)
      o3          := v2(sel)
    val top = (new Bar).dropStructsVecs
    assertCodeString(
      top,
      """|val arg: Bits[32] <> CONST = (h"01", h"02", h"03", h"04").toBits
         |val arg2: Bits[128] <> CONST =
         |  (
         |    (h"01", h"02", h"03", h"04").toBits, (h"05", h"06", h"07", h"08").toBits,
         |    (h"09", h"10", h"11", h"12").toBits, (h"13", h"14", h"15", h"16").toBits
         |  ).toBits
         |
         |class Bar extends RTDesign:
         |  val o = Bits(8) <> OUT
         |  val o2 = Bits(32) <> OUT
         |  val o3 = Bits(32) <> OUT
         |  val v = Bits(8) X 4 <> VAR init arg.as(Bits(8) X 4)
         |  val v2 = Bits(32) X 4 <> VAR init arg2.as(Bits(32) X 4)
         |  val v3 = Bits(8) X 4 <> VAR.REG
         |  val sel = UInt(2) <> IN
         |  o := ((arg(31, 24) ^ arg(23, 16)) ^ v(sel.toInt)) ^ v3(sel.toInt)
         |  o2 := arg2(127, 96)
         |  v3(sel.toInt).din := v(sel.toInt)
         |  o3 := v2(sel.toInt)
         |end Bar
         |""".stripMargin
    )
  }

  test("Inline anomaly") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    val Rcon: Bits[8] X 4 X 2 <> CONST    = DFVector(Bits(8) X 4 X 2)(
      DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04"),
      DFVector(Bits(8) X 4)(h"05", h"06", h"07", h"08")
    )

    extension (lhs: Bits[8] X 4 <> VAL)
      @inline def +++(rhs: Bits[8] X 4 <> VAL): Bits[8] X 4 <> DFRET =
        lhs.elements.lazyZip(rhs.elements).map(_ ^ _).toList

    class Bar extends RTDesign:
      val o = Bits[8] X 4 <> OUT
      o := Rcon(0) +++ Rcon(0)
    val top = (new Bar).dropStructsVecs
    assertCodeString(
      top,
      """|val Rcon: Bits[64] <> CONST = ((h"01", h"02", h"03", h"04").toBits, (h"05", h"06", h"07", h"08").toBits).toBits
         |
         |class Bar extends RTDesign:
         |  val o = Bits(32) <> OUT
         |  o := (Rcon(63, 56) ^ Rcon(63, 56), Rcon(55, 48) ^ Rcon(55, 48), Rcon(47, 40) ^ Rcon(47, 40), Rcon(39, 32) ^ Rcon(39, 32)).toBits
         |end Bar
         |""".stripMargin
    )
  }
end DropStructsVecsSpec
