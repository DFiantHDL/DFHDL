package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitCondExprAssign
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitCondExprAssignSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Conditional expression assignment") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      z := (
        if (x > 0) 5
        else if (x < 0) x + 1
        else x
      )
      val z2 = SInt(16) <> VAR
      z2 := z match
        case 1 | 2 => 17
        case _     => z + 12
      y := z2
    val id = (new ID).explicitCondExprAssign
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  if (x > sd"16'0") z := sd"16'5"
         |  else if (x < sd"16'0") z := x + sd"16'1"
         |  else z := x
         |  val z2 = SInt(16) <> VAR
         |  z match
         |    case sd"16'1" | sd"16'2" => z2 := sd"16'17"
         |    case _ => z2 := z + sd"16'12"
         |  end match
         |  y := z2
         |end ID
         |""".stripMargin
    )
  }
  test("Nested conditional expression assignment") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      z := (
        if (x > 0)
          if (x > 5) 5
          else -5
        else if (x < 0) x + 1
        else x
      )
      val z2 = SInt(16) <> VAR
      z2 := z match
        case 1 | 2 =>
          val zz = SInt(4) <> VAR
          zz := z match
            case 1 => 5
            case 2 => 3
          if (x < 11) zz + 3
          else zz
        case _ => z + 12
      y := z
    end ID
    val id = (new ID).explicitCondExprAssign
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  if (x > sd"16'0")
         |    if (x > sd"16'5") z := sd"16'5"
         |    else z := sd"16'-5"
         |  else if (x < sd"16'0") z := x + sd"16'1"
         |  else z := x
         |  end if
         |  val z2 = SInt(16) <> VAR
         |  z match
         |    case sd"16'1" | sd"16'2" =>
         |      val zz = SInt(4) <> VAR
         |      z match
         |        case sd"16'1" => zz := sd"4'5"
         |        case sd"16'2" => zz := sd"4'3"
         |      end match
         |      if (x < sd"16'11") z2 := (zz +^ sd"4'3").resize(16)
         |      else z2 := zz.resize(16)
         |    case _ => z2 := z + sd"16'12"
         |  end match
         |  y := z
         |end ID""".stripMargin
    )
  }

  test("AES xtime example") {
    class xtime extends DFDesign:
      val lhs     = Bits(8) <> IN
      val shifted = Bits(8) <> VAR
      shifted := lhs << 1
      val o = Bits(8) <> OUT
      o <>
        ((
          if (lhs(7)) shifted ^ h"1b"
          else shifted
        ): Bits[8] <> VAL)
    end xtime
    val id = (new xtime).explicitCondExprAssign
    assertCodeString(
      id,
      """|class xtime extends DFDesign:
         |  val lhs = Bits(8) <> IN
         |  val shifted = Bits(8) <> VAR
         |  shifted := lhs << 1
         |  val o = Bits(8) <> OUT
         |  if (lhs(7)) o := shifted ^ h"1b"
         |  else o := shifted
         |end xtime""".stripMargin
    )
  }

  test("LRShiftFlat example") {
    enum ShiftDir extends Encoded:
      case Left, Right

    class LRShiftFlat(
        val width: Int <> CONST = 8
    ) extends RTDesign:
      val iBits = Bits(width)       <> IN
      val shift = UInt.until(width) <> IN
      val dir   = ShiftDir          <> IN
      val oBits = Bits(width)       <> OUT
      oBits := dir match
        case ShiftDir.Left  => iBits << shift
        case ShiftDir.Right => iBits >> shift
    end LRShiftFlat
    val id = (new LRShiftFlat).explicitCondExprAssign
    assertCodeString(
      id,
      """|enum ShiftDir(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |  case Left extends ShiftDir(d"1'0")
         |  case Right extends ShiftDir(d"1'1")
         |
         |class LRShiftFlat(val width: Int <> CONST = 8) extends RTDesign:
         |  val iBits = Bits(width) <> IN
         |  val shift = UInt(clog2(width)) <> IN
         |  val dir = ShiftDir <> IN
         |  val oBits = Bits(width) <> OUT
         |  dir match
         |    case ShiftDir.Left => oBits := iBits << shift.toInt
         |    case ShiftDir.Right => oBits := iBits >> shift.toInt
         |  end match
         |end LRShiftFlat""".stripMargin
    )
  }
  test("ED domain conditional expression connection outside process") {
    class Top extends EDDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      y <>
        ((
          if (x(7)) x ^ h"1b"
          else x
        ): Bits[8] <> VAL)
    end Top
    val result = (new Top).explicitCondExprAssign
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  process(all):
         |    if (x(7)) y := x ^ h"1b"
         |    else y := x
         |end Top
         |""".stripMargin
    )
  }

  // the `process(all)` wrap of the previous test exists because an ED design body is concurrent
  // and holds no statements. An HDL method body is procedural, so the conditional statement stays
  // where it is (a process inside a method is illegal).
  test("ED method body takes a conditional statement without a process wrap") {
    class Top extends EDDesign:
      val a, b                                                         = UInt(8) <> IN
      val c                                                            = Bit     <> IN
      val y                                                            = UInt(8) <> OUT
      def pick(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
        if (c) l
        else r
      y <> pick(a, b)
    end Top
    val result = (new Top).explicitCondExprAssign
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> IN
         |  val c = Bit <> IN
         |  val y = UInt(8) <> OUT
         |  def pick(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
         |    val anon = UInt(8) <> VAR
         |    if (c) anon := l
         |    else anon := r
         |    anon
         |  end pick
         |
         |  y <> pick(a, b)
         |end Top
         |""".stripMargin
    )
  }

  test("ED domain conditional expression connection to child input port") {
    class Child extends EDDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      y <> x
    class Top extends EDDesign:
      val a          = Bits(8) <> IN
      val b          = Bits(8) <> OUT
      val child_inst = Child()
      child_inst.x <>
        ((
          if (a(7)) a ^ h"1b"
          else a
        ): Bits[8] <> VAL)
      b <> child_inst.y
    end Top
    val result = (new Top).explicitCondExprAssign
    assertCodeString(
      result,
      """|class Child extends EDDesign:
         |  val x = Bits(8) <> IN
         |  val y = Bits(8) <> OUT
         |  y <> x
         |end Child
         |
         |class Top extends EDDesign:
         |  val a = Bits(8) <> IN
         |  val b = Bits(8) <> OUT
         |  val child_inst = Child()
         |  val x_part = Bits(8) <> VAR
         |  process(all):
         |    if (a(7)) x_part := a ^ h"1b"
         |    else x_part := a
         |  child_inst.x <> x_part
         |  b <> child_inst.y
         |end Top
         |""".stripMargin
    )
  }

end ExplicitCondExprAssignSpec
