package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropLocalDcls
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropLocalDclsSpec extends StageSpec:
  test("Nested local dcl move"):
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      z := x
      if (x > 5)
        val zz = SInt(16) <> VAR
        x match
          case 2 =>
            val zzz                  = SInt(16) <> VAR init 0
            val c: SInt[16] <> CONST = 1
            zzz := zzz.prev(1) + c
          case _ =>
        zz := x
        z  := zz
      y := z
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  z := x
         |  val zz = SInt(16) <> VAR
         |  val zzz = SInt(16) <> VAR init sd"16'0"
         |  val c: SInt[16] <> CONST = sd"16'1"
         |  if (x > sd"16'5")
         |    x match
         |      case sd"16'2" => zzz := zzz.prev + c
         |      case _ =>
         |    end match
         |    zz := x
         |    z := zz
         |  end if
         |  y := z
         |end ID
         |""".stripMargin
    )

  test("Process also drops local dcls"):
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      process(all):
        if (x > 5)
          val zz = SInt(16) <> VAR
          x match
            case 2 =>
              val zzz                  = SInt(16) <> VAR init 0
              val c: SInt[16] <> CONST = 1
              zzz := x + c
            case _ =>
          zz := x
          z  := zz
        y := z
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  val zz = SInt(16) <> VAR
         |  val zzz = SInt(16) <> VAR init sd"16'0"
         |  val c: SInt[16] <> CONST = sd"16'1"
         |  process(all):
         |    zz := ?
         |    if (x > sd"16'5")
         |      x match
         |        case sd"16'2" => zzz := x + c
         |        case _ =>
         |      end match
         |      zz := x
         |      z := zz
         |    end if
         |    y := z
         |end ID
         |""".stripMargin
    )
  test("Process keeps local dcls under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      process(all):
        if (x > 5)
          val zz = SInt(16) <> VAR
          x match
            case 2 =>
              val zzz                  = SInt(16) <> VAR init 0
              val c: SInt[16] <> CONST = 1
              zzz := x + c
            case _ =>
          zz := x
          z  := zz
        y := z
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  process(all):
         |    val zz = SInt(16) <> VAR
         |    zz := ?
         |    val zzz = SInt(16) <> VAR init sd"16'0"
         |    val c: SInt[16] <> CONST = sd"16'1"
         |    if (x > sd"16'5")
         |      x match
         |        case sd"16'2" => zzz := x + c
         |        case _ =>
         |      end match
         |      zz := x
         |      z := zz
         |    end if
         |    y := z
         |end ID
         |""".stripMargin
    )

  test("Process moves REG local dcls to design level under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process:
        val zzReg = SInt(16) <> VAR.REG init 0
        val zz    = SInt(16) <> VAR
        zzReg.din := x
        zz        := zzReg
        y         := zz
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val zzReg = SInt(16) <> VAR.REG init sd"16'0"
         |  process:
         |    val zz = SInt(16) <> VAR
         |    zzReg.din := x
         |    zz := zzReg
         |    y := zz
         |end ID
         |""".stripMargin
    )

  test("Step block drops local dcls"):
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG
      process:
        def S_0: Step =
          val zz = SInt(16) <> VAR
          zz    := x
          y.din := zz
          NextStep
        end S_0
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT.REG
         |  val zz = SInt(16) <> VAR
         |  process:
         |    def S_0: Step =
         |      zz := x
         |      y.din := zz
         |      NextStep
         |    end S_0
         |end ID
         |""".stripMargin
    )

  test("Step block keeps local dcls under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG
      process:
        def S_0: Step =
          val zz = SInt(16) <> VAR
          zz    := x
          y.din := zz
          NextStep
        end S_0
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT.REG
         |  process:
         |    val zz = SInt(16) <> VAR
         |    def S_0: Step =
         |      zz := x
         |      y.din := zz
         |      NextStep
         |    end S_0
         |end ID
         |""".stripMargin
    )

  test("Combinational defaults for conditionally scoped dcls"):
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process(all):
        y := 0
        while (y < x)
          val zw = SInt(16) <> VAR
          zw := y + 1
          y  := zw
        for (i <- 0 until 4)
          if (x > 5)
            val zf = SInt(16) <> VAR
            zf := x + i
            y  := zf
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val zw = SInt(16) <> VAR
         |  val zf = SInt(16) <> VAR
         |  process(all):
         |    y := sd"16'0"
         |    zw := ?
         |    while (y < x)
         |      zw := y + sd"16'1"
         |      y := zw
         |    end while
         |    zf := ?
         |    for (i <- 0 until 4)
         |      if (x > sd"16'5")
         |        zf := x + sd"16'${i}"
         |        y := zf
         |      end if
         |    end for
         |end ID
         |""".stripMargin
    )

  test("No combinational defaults in non-combinational processes"):
    class ID extends EDDesign:
      val clk = Bit      <> IN
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val z   = SInt(16) <> OUT
      process(clk):
        if (clk.rising)
          val zc = SInt(16) <> VAR
          zc := x + 1
          y  := zc
      process(x):
        if (x > 5)
          val zs = SInt(16) <> VAR
          zs := x + 1
          z  := zs
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val clk = Bit <> IN
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> OUT
         |  val zc = SInt(16) <> VAR
         |  process(clk):
         |    if (clk.rising)
         |      zc := x + sd"16'1"
         |      y := zc
         |    end if
         |  val zs = SInt(16) <> VAR
         |  process(x):
         |    if (x > sd"16'5")
         |      zs := x + sd"16'1"
         |      z := zs
         |    end if
         |end ID
         |""".stripMargin
    )

  test("Loop block drops local dcls"):
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process(all):
        y := 0
        for (i <- 0 until 4)
          val zz = SInt(16) <> VAR
          zz := x + i
          y  := y + zz
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val zz = SInt(16) <> VAR
         |  process(all):
         |    y := sd"16'0"
         |    for (i <- 0 until 4)
         |      zz := x + sd"16'${i}"
         |      y := y + zz
         |    end for
         |end ID
         |""".stripMargin
    )

  test("Loop block keeps local dcls in the process under VHDL"):
    given options.CompilerOptions.Backend = _.vhdl
    class ID extends EDDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      process(all):
        y := 0
        for (i <- 0 until 4)
          val zz = SInt(16) <> VAR
          zz := x + i
          y  := y + zz
    end ID
    val id = (new ID).dropLocalDcls
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  process(all):
         |    y := sd"16'0"
         |    val zz = SInt(16) <> VAR
         |    for (i <- 0 until 4)
         |      zz := x + sd"16'${i}"
         |      y := y + zz
         |    end for
         |end ID
         |""".stripMargin
    )
end DropLocalDclsSpec
