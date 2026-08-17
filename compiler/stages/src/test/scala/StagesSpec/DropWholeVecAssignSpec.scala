package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropWholeVecAssign
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropWholeVecAssignSpec extends StageSpec(stageCreatesUnrefAnons = true):
  given options.CompilerOptions.Backend = _.verilog.v2001

  test("uniform init becomes an initial block loop"):
    class Top extends EDDesign:
      val x   = Bits(8)     <> IN
      val y   = Bits(8)     <> OUT
      val mem = Bits(8) X 4 <> VAR init all(all(0))
      process(all):
        mem(0) :== x
        y :== mem(3)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X 4 <> VAR
         |  val mem_init = initial:
         |    for (mem_i <- 0 until 4)
         |      mem(mem_i) := h"00"
         |    end for
         |  process(all):
         |    mem(0) :== x
         |    y :== mem(3)
         |end Top
         |""".stripMargin
    )

  test("uniform connection becomes per-cell connections"):
    class Top extends EDDesign:
      val y   = Bits(8)     <> OUT
      val con = Bits(8) X 4 <> VAR
      con <> all(all(0))
      y   <> con(2)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = Bits(8) <> OUT
         |  val con = Bits(8) X 4 <> VAR
         |  con(0) <> h"00"
         |  con(1) <> h"00"
         |  con(2) <> h"00"
         |  con(3) <> h"00"
         |  y <> con(2)
         |end Top
         |""".stripMargin
    )

  test("uniform assignment becomes a loop in place"):
    class Top extends EDDesign:
      val x   = Bit         <> IN
      val y   = Bits(8)     <> OUT
      val mem = Bits(8) X 4 <> VAR
      process(all):
        if (x) mem :== all(all(0))
        else mem(0) :== h"ff"
        y :== mem(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bit <> IN
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X 4 <> VAR
         |  process(all):
         |    if (x)
         |      for (mem_i <- 0 until 4)
         |        mem(mem_i) :== h"00"
         |      end for
         |    else mem(0) :== h"ff"
         |    end if
         |    y :== mem(1)
         |end Top
         |""".stripMargin
    )

  test("differing cells unroll"):
    class Top extends EDDesign:
      val y   = Bits(8)     <> OUT
      val lut = Bits(8) X 4 <> VAR init Vector(h"01", h"02", h"03", h"04")
      y <> lut(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = Bits(8) <> OUT
         |  val lut = Bits(8) X 4 <> VAR
         |  val lut_init = initial:
         |    lut(0) := h"01"
         |    lut(1) := h"02"
         |    lut(2) := h"03"
         |    lut(3) := h"04"
         |  y <> lut(1)
         |end Top
         |""".stripMargin
    )

  test("a named constant source unrolls into cell selections"):
    class Top extends EDDesign:
      val TBL: Bits[8] X 4 <> CONST = Vector(h"01", h"02", h"03", h"04")
      val y                         = Bits(8)     <> OUT
      val rom                       = Bits(8) X 4 <> VAR init TBL
      y <> rom(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val TBL: Bits[8] X 4 <> CONST = DFVector(Bits(8) X 4)(h"01", h"02", h"03", h"04")
         |  val y = Bits(8) <> OUT
         |  val rom = Bits(8) X 4 <> VAR
         |  val rom_init = initial:
         |    rom(0) := TBL(0)
         |    rom(1) := TBL(1)
         |    rom(2) := TBL(2)
         |    rom(3) := TBL(3)
         |  y <> rom(1)
         |end Top
         |""".stripMargin
    )

  // any other constant source is taken apart by selecting each cell out of it, which covers the
  // `Bits`-to-vector cast the Verilog printer used to special-case
  test("a bits-to-vector cast source unrolls into cell selections"):
    class Top extends EDDesign:
      val SRC: Bits[32] <> CONST = h"01020304"
      val y                      = Bits(8)     <> OUT
      val mem                    = Bits(8) X 4 <> VAR init SRC.as(Bits(8) X 4)
      y <> mem(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val SRC: Bits[32] <> CONST = h"01020304"
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X 4 <> VAR
         |  val mem_init = initial:
         |    mem(0) := SRC.as(Bits(8) X 4)(0)
         |    mem(1) := SRC.as(Bits(8) X 4)(1)
         |    mem(2) := SRC.as(Bits(8) X 4)(2)
         |    mem(3) := SRC.as(Bits(8) X 4)(3)
         |  y <> mem(1)
         |end Top
         |""".stripMargin
    )

  test("a parametric length keeps the parameter as the loop bound"):
    class Top(val N: Int <> CONST = 4) extends EDDesign:
      val y   = Bits(8)     <> OUT
      val mem = Bits(8) X N <> VAR init all(all(0))
      y <> mem(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top(val N: Int <> CONST = 4) extends EDDesign:
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X N <> VAR
         |  val mem_init = initial:
         |    for (mem_i <- 0 until N)
         |      mem(mem_i) := h"00"
         |    end for
         |  y <> mem(1)
         |end Top
         |""".stripMargin
    )

  // the operands need not be constant: the loop body reads them exactly where the whole-vector
  // form did
  test("a non-constant uniform source still becomes a loop"):
    class Top extends EDDesign:
      val x   = Bits(8)     <> IN
      val y   = Bits(8)     <> OUT
      val mem = Bits(8) X 4 <> VAR
      process(all):
        mem :== all(x)
        y :== mem(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X 4 <> VAR
         |  process(all):
         |    for (mem_i <- 0 until 4)
         |      mem(mem_i) :== x
         |    end for
         |    y :== mem(1)
         |end Top
         |""".stripMargin
    )

  test("a non-constant per-cell source unrolls"):
    class Top extends EDDesign:
      val x   = Bits(8)     <> IN
      val y   = Bits(8)     <> OUT
      val cat = Bits(8) X 2 <> VAR
      process(all):
        cat :== Vector(x, x | h"01")
        y :== cat(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val cat = Bits(8) X 2 <> VAR
         |  process(all):
         |    cat(0) :== x
         |    cat(1) :== x | h"01"
         |    y :== cat(1)
         |end Top
         |""".stripMargin
    )

  // a NAMED non-constant source is a plain vector-to-vector drive, which is left alone
  test("a named non-constant source is left alone"):
    class Top extends EDDesign:
      val y   = Bits(8)     <> OUT
      val src = Bits(8) X 4 <> VAR
      val mem = Bits(8) X 4 <> VAR
      process(all):
        mem :== src
        y :== mem(1)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = Bits(8) <> OUT
         |  val src = Bits(8) X 4 <> VAR
         |  val mem = Bits(8) X 4 <> VAR
         |  process(all):
         |    mem :== src
         |    y :== mem(1)
         |end Top
         |""".stripMargin
    )

  test("a vector that the backend flattens is left alone"):
    // `v` is read whole, so `DropStructsVecs` will flatten it into Bits and the whole-vector
    // drive becomes a plain (legal) bit-vector drive; unrolling it would only leave cell
    // selections for the flattening to turn into variable-bound part-selects
    class Top extends EDDesign:
      val y = Bits(32)    <> OUT
      val v = Bits(8) X 4 <> VAR
      process(all):
        v :== all(all(0))
        y :== v.bits
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = Bits(32) <> OUT
         |  val v = Bits(8) X 4 <> VAR
         |  process(all):
         |    v :== all(h"00")
         |    y :== v.bits
         |end Top
         |""".stripMargin
    )

  // Only the declaration's OWN (outermost) dimension is unrolled. Under the older dialects
  // `DropStructsVecs` then flattens the cell type to `Bits`, making the cell drive a plain
  // bit-vector drive, and in SystemVerilog a cell-level array literal is legal as it stands.
  test("a multi-dimensional vector unrolls its outermost dimension only"):
    class Top extends EDDesign:
      val x   = Bits(8)         <> IN
      val y   = Bits(8)         <> OUT
      val mem = Bits(8) X 4 X 2 <> VAR init all(all(all(0)))
      val con = Bits(8) X 4 X 2 <> VAR
      con <> all(all(all(0)))
      process(all):
        mem :== all(all(x))
        y :== mem(1)(2) | con(0)(3)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  val mem = Bits(8) X 4 X 2 <> VAR
         |  val mem_init = initial:
         |    for (mem_i <- 0 until 2)
         |      mem(mem_i) := all(h"00")
         |    end for
         |  val con = Bits(8) X 4 X 2 <> VAR
         |  con(0) <> all(h"00")
         |  con(1) <> all(h"00")
         |  process(all):
         |    for (mem_i <- 0 until 2)
         |      mem(mem_i) :== all(x)
         |    end for
         |    y :== mem(1)(2) | con(0)(3)
         |end Top
         |""".stripMargin
    )

  test("a multi-dimensional per-cell source unrolls its outermost dimension only"):
    class Top extends EDDesign:
      val y   = Bits(8)         <> OUT
      val lut = Bits(8) X 2 X 2 <> VAR init Vector(Vector(h"01", h"02"), Vector(h"03", h"04"))
      y <> lut(1)(0)
    end Top
    val top = (new Top).dropWholeVecAssign
    assertCodeString(
      top,
      """|class Top extends EDDesign:
         |  val y = Bits(8) <> OUT
         |  val lut = Bits(8) X 2 X 2 <> VAR
         |  val lut_init = initial:
         |    lut(0) := DFVector(Bits(8) X 2)(h"01", h"02")
         |    lut(1) := DFVector(Bits(8) X 2)(h"03", h"04")
         |  y <> lut(1)(0)
         |end Top
         |""".stripMargin
    )

end DropWholeVecAssignSpec
