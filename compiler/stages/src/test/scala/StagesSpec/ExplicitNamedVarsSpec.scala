package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitNamedVars
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitNamedVarsSpec extends StageSpec:
  test("Basic named variable") {
    class ID extends DFDesign:
      val x                    = SInt(16) <> IN
      val y                    = SInt(16) <> OUT
      val z                    = x + 1
      val c: SInt[16] <> CONST = 11
      val f                    = c + c
      y := z + f
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  z := x + sd"16'1"
         |  val c: SInt[16] <> CONST = sd"16'11"
         |  val f: SInt[16] <> CONST = c + c
         |  y := z + f
         |end ID
         |""".stripMargin
    )
  }
  test("Named conditional expression") {
    class ID extends DFDesign:
      val x                  = SInt(16) <> IN
      val y                  = SInt(16) <> OUT
      val z: SInt[16] <> VAL =
        if (x > 0) 5
        else if (x < 0) x + 1
        else x
      val z2: SInt[16] <> VAL =
        z match
          case 1 | 2 => 17
          case _     => z + 12
      y := z2
    val id = (new ID).explicitNamedVars
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
  test("Nested named conditional expression") {
    class ID extends DFDesign:
      val x                  = SInt(16) <> IN
      val y                  = SInt(16) <> OUT
      val z: SInt[16] <> VAL =
        if (x > 0)
          if (x > 5) 5
          else -5
        else if (x < 0) x + 1
        else x
      val z2: SInt[16] <> VAL =
        z match
          case 1 | 2 =>
            val zz: SInt[4] <> VAL = z match
              case 1 => 5
              case 2 => 3
            if (x < 11) zz + 3
            else zz
          case _ => z + 12
      y := z
    end ID
    val id = (new ID).explicitNamedVars
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
         |end ID
         |""".stripMargin
    )
  }

  test("AES xtime example") {
    class xtime extends DFDesign:
      val lhs     = Bits(8) <> IN
      val shifted = lhs << 1
      val o       = Bits(8) <> OUT
      o <> (
        if (lhs(7)) shifted ^ h"1b"
        else shifted
      )
    end xtime
    val id = (new xtime).explicitNamedVars
    assertCodeString(
      id,
      """|class xtime extends DFDesign:
         |  val lhs = Bits(8) <> IN
         |  val shifted = Bits(8) <> VAR
         |  shifted := lhs << 1
         |  val o = Bits(8) <> OUT
         |  o <> ((
         |    if (lhs(7)) shifted ^ h"1b"
         |    else shifted
         |  ): Bits[8] <> VAL)
         |end xtime""".stripMargin
    )
  }

  test("Simple named ident") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val v = x
      y := v
    end ID
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  v := x
         |  y := v
         |end ID
         |""".stripMargin
    )
  }

  test("RT process block named values") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0
      process:
        def S0: Step =
          val v = x
          y.din := v
          NextStep
        val vr       = x
        def S1: Step =
          y.din := vr + 1
          NextStep
        val vrc      = (if (x > 5) x else x + 1)
        def S2: Step =
          y.din := vrc
          NextStep
        val vm = x
        y.din := vm
        def S3: Step =
          y.din := vm
          NextStep
        val vrcm = (if (x > 5) x else x + 1)
        y.din := vrcm
        def S4: Step =
          y.din := vrcm
          NextStep
    end ID
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT.REG init sd"16'0"
         |  process:
         |    def S0: Step =
         |      val v = SInt(16) <> VAR
         |      v := x
         |      y.din := v
         |      NextStep
         |    end S0
         |    val vr = SInt(16) <> VAR.REG
         |    vr.din := x
         |    def S1: Step =
         |      y.din := vr + sd"16'1"
         |      NextStep
         |    end S1
         |    val vrc = SInt(16) <> VAR.REG
         |    if (x > sd"16'5") vrc.din := x
         |    else vrc.din := x + sd"16'1"
         |    def S2: Step =
         |      y.din := vrc
         |      NextStep
         |    end S2
         |    val vm_din = SInt(16) <> VAR
         |    val vm = SInt(16) <> VAR.REG
         |    vm_din := x
         |    vm.din := vm_din
         |    y.din := vm_din
         |    def S3: Step =
         |      y.din := vm
         |      NextStep
         |    end S3
         |    val vrcm_din = SInt(16) <> VAR
         |    val vrcm = SInt(16) <> VAR.REG
         |    if (x > sd"16'5") vrcm_din := x
         |    else vrcm_din := x + sd"16'1"
         |    y.din := vrcm_din
         |    def S4: Step =
         |      y.din := vrcm
         |      NextStep
         |    end S4
         |end ID""".stripMargin
    )
  }
  test("EDDomain rules") {
    class ID extends EDDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val py = SInt(16) <> OUT
      val v  = x
      y <> v
      process:
        val pv = x
        py := pv
    end ID
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val py = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  v <> x
         |  y <> v
         |  process:
         |    val pv = SInt(16) <> VAR
         |    pv := x
         |    py := pv
         |end ID""".stripMargin
    )
  }

  // TODO: this causes an exception
  // test("AES regression test") {
  //   case class AESByte() extends Opaque(Bits(8))

  //   class xtime extends DFDesign:
  //     val lhs = AESByte  <> IN
  //     val o   = AESByte  <> OUT
  //     val shifted = lhs.bits << 1
  //     val anon: Bits[8]  <> VAL =
  //       if (lhs.bits(7))
  //         val o: Bits[8] <> CONST = h"1b"
  //         shifted ^ o
  //       else shifted
  //     o <> anon.as(AESByte)
  //   end xtime

  //   val top = (new xtime).explicitNamedVars
  //   assertCodeString(
  //     top,
  //     """|""".stripMargin
  //   )
  // }
end ExplicitNamedVarsSpec
